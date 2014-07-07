(ns discharge.config
  (:require [discharge.utils :refer :all]
            [discharge.cache :as cache]
            [clojure.java.io :as jio]
            [clojure.tools.logging :as log]))

(def config-file (atom nil))

(def ^:private
  defaults {:in-dir "resources/"
            :out-dir "html/"
            :default-template "default.clj"
            :default-extension "html"
            :encoding "UTF-8"
            :posts-per-page 2
            :blog-as-index true
            :create-archives true})

(defn- absolutize-paths [config root-dir]
  (reduce (fn [conf p-key]
            (update-in conf [p-key]
                       #(str (jio/file root-dir %))))
          config
          [:in-dir :out-dir]))

(defn- validate-config [config]
  (cond (not (:site-title config))
        (fail ":site-title is not defined.")

        (not (:site-url config))
        (fail ":site-url is not defined.")

        (and (:emacs config) (not (.exists (jio/file (:emacs config)))))
        (fail "Path to Emacs is not valid.")

        :else config))

(defn config []
  (or (cache/lookup @config-file)
      (let [config (-> (slurp @config-file)
                       read-string
                       (->> (apply hash-map)
                            (merge defaults))
                       (assoc :root-dir (.getParent @config-file))
                       (absolutize-paths (.getParent @config-file)))]
        (cache/add @config-file config)
        config)))

(defn set-config-file! [f]
  (reset! config-file f))

