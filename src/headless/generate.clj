(ns headless.generate
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as string]))

(defn lisp-symbol
  "Converts the string `name` to a lower-case, lisp-cased symbol.
  Dots are replaced with dashes and CamelCase is converted to lisp-case."
  [name]
  (-> name
      (string/replace #"[.]" "-")
      (string/replace #"([A-Z]+)([A-Z][a-z])" "$1-$2")
      (string/replace #"([a-z])([A-Z])" "$1-$2")
      (string/lower-case)
      (symbol)))

(defn format-docstring
  "Returns docstring for `entity` (command or event).
  To improve readability, the line length of the description part of the
  docstring is limited to 80 characters.
  In addition to the shared fields `description`, `experimental`, `deprecated`
  and `parameters`, commands also contain a `return` value specification."
  [entity]
  (let [{:keys [parameters returns description experimental deprecated]} entity
        description (->> (string/replace (or description "") #"\n" " ")
                         (re-seq #".{1,80}\\s|.{1,80}")
                         (string/join "\n"))
        info (str (if experimental "(experimental) ")
                  (if deprecated "(deprecated)"))]
    (str description "\n"
         (if-not (string/blank? info) (str "Info: " info "\n"))
         "\nParamters:\n"
         (with-out-str (pprint/pprint parameters))
         (if returns
           (str "\nReturns:\n" (with-out-str (pprint/pprint returns)))))))

(defn format-command-definition
  "Returns function definition of `command` from `domain`.
  Function name is in the format: domain-command-name.
  The defined function takes a `connection` and optionally keyword arguments
  representing the parameters for the command. Returns a promise for the result.
  See `core/execute`."
  [domain command]
  (let [name (str (:domain domain) "." (:name command))
        docstring (format-docstring command)
        keys (mapv (comp symbol :name) (:parameters command))
        params (if (not-empty keys)
                 `[~'connection ~'& {:keys ~keys :as ~'params}]
                 `[~'connection])
        execute-params-map (if (not-empty keys) 'params {})]
    `(defn ~(lisp-symbol name) ~docstring ~params
       (~'execute ~'connection ~name ~execute-params-map))))

(defn format-event-definition
  "Returns function definition of `event` from `domain`.
  Function name is in the format: domain-on-event-name.
  The defined function takes a `connection` and a one-arity handler function.
  The function registers the handler on the provided connection for `event` and
  returns a zero-arity function that can be called to remove the handler again.
  See `core/register-event-handler`."
  [domain event]
  (let [name (lisp-symbol (str (:domain domain) ".on." (:name event)))
        event-name (str (:domain domain) "." (:name event))
        metadata {:event-name event-name}
        docstring (format-docstring event)]
    `(defn ~name ~docstring ~metadata [~'connection ~'handler]
       (~'register-event-handler ~'connection ~event-name ~'handler))))

;; TODO improve performance
;; especially the docstring generation using pretty print takes quite some time
;; spit to file? but then we run into a circular dependency problem with core...
(defmacro commands-and-events
  "Create command and event definitions for protocol specification at `path`."
  [path]
  (let [{domains :domains} (-> (io/resource path)
                               (slurp)
                               (json/read-str :key-fn keyword))
        events (for [domain domains event (:events domain)]
                 (format-event-definition domain event))
        commands (for [domain domains command (:commands domain)]
                   (format-command-definition domain command))]
    `(do ~@commands
         ~@events)))
