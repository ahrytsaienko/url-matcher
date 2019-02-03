(ns url-matcher.core
  (:require [clojure.string :as str]))


(def URL-RE #"^(https|http):\/\/([\w-\.+]+)/?([\w-/]*)\??(.*)")


(defn split-str [string delimiter]
  (->> (str/split string delimiter)
    (remove str/blank?)
    (map str/trim)))


(defn assoc-query-params [query-params v]
  (let [[k v] (split-str v #"=")]
    (assoc query-params (keyword k) (subs v 1))))


(defn path-params->recognize [path-pattern path-parts]
  (let [values (take (max (count path-pattern) (count path-parts))
                 (map #(vector %1 %2)
                   (concat path-pattern (repeat nil))
                   (concat path-parts (repeat nil))))]
    (reduce (fn [accum [pattern v]]
              (cond
                (or (nil? pattern) (nil? v)) (reduced nil)
                (keyword? pattern) (conj accum [pattern v])
                (string? pattern) accum
                :else (reduced nil))) [] values)))


(defn recognize [{:keys [host-pattern path-pattern qp-pattern]} url]
  (let [[_ _ host path-params query-params] (re-find URL-RE url)]
    (when (= host host-pattern)
      (let [path-parts (split-str path-params #"/")
            binds      (if (seq path-parts)
                         (path-params->recognize path-pattern path-parts)
                         [])
            qp         (when-not (str/blank? query-params)
                         (->> (split-str query-params #"&")
                           (map #(str/split % #"="))
                           (into {})))]
        (reduce (fn [accum [k alias]]
                  (if-some [v (get qp (name k))]
                    (conj accum [(keyword alias) v])
                    (reduced nil)))
          binds qp-pattern)))))


(defn url-pattern [string]
  {:pre [(string? string)] :post (map? %)}
  (loop [split   (split-str string #";")
         pattern {}]
    (if (seq split)
      (recur (rest split)
        (let [[_ p-type value] (re-find #"(\w+)\((.+)\)" (first split))]
          (case p-type
            "host" (assoc pattern :host-pattern value)
            "path" (assoc pattern :path-pattern (map #(if (= (subs % 0 1) "?")
                                                       (keyword (subs % 1))
                                                       %) (split-str value #"/")))
            "queryparam" (update pattern :qp-pattern assoc-query-params value))))
      pattern)))


(defn -main
  [& args]
  nil)
