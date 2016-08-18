(ns discharge.io
  (:require [discharge.cache :as cache]
            [discharge.org :as org]
            [discharge.config :refer [config]]
            [discharge.utils :refer :all]
            [clojure.java.io :as jio]
            [hiccup.core :as hiccup]
            [clojure.tools.logging :as log]
            [clojure.string :as s]
            [clojure.java.shell :refer [sh]])
  (:import (org.pegdown PegDownProcessor)
           (org.apache.commons.io FileUtils FilenameUtils)))

(defn slurp-file [f]
  (slurp f :encoding (:encoding (config))))

(defn- split-file [content]
  (let [idx (.indexOf content "---" 4)]
    [(.substring content 4 idx) (.substring content (+ 3 idx))]))

(defn- parse-doc-tags [metadata]
  (let [tagstring (:tags metadata)]
    (if (string? tagstring)
      (assoc metadata :tags (into #{} (.split (:tags metadata) " ")))
      metadata)))

(defn- prepare-metadata [metadata]
  (parse-doc-tags
   (reduce (fn [tags [_ k v]]
             (assoc tags (keyword k) v))
           {} (re-seq #"([^:#\+]+): (.+)(\n|$)" metadata))))

(defn get-date-from-filename [file]
  (when-let [date (re-find #"\d*-\d*-\d*" (str file))]
    (format-date date "yyyy-MM-dd")))

(defn- read-markdown [file]
  (let [[metadata content]
        (split-file (slurp-file file))]
    [(prepare-metadata metadata)
     (.markdownToHtml (PegDownProcessor.) content)]))

(defn- read-html [file]
  (let [[metadata content]
        (split-file (slurp-file))]
    [(prepare-metadata metadata) content]))

(defn- read-org [file]
  (let [[synopsis content] (org/read-org-file file)]
    (as-> (s/join (take 500 (slurp-file file))) metadata
          (prepare-metadata metadata)
          (assoc metadata :date (get-date-from-filename file))
          (cond-> metadata
           (not (:nocut metadata)) (assoc :synopsis synopsis))
          [metadata content])))

(defn- read-clj [file]
  (let [[metadata content] (read-string
                            (str "(" (slurp-file file) ")"))]
    [metadata (binding [*ns* (find-ns 'discharge.core)]
                (hiccup/html (eval content)))]))

(defn read-doc [f]
  (if-let [cached-doc (cache/lookup f)]
    cached-doc
    (let [extension (FilenameUtils/getExtension (str f))
          doc (cond (#{"markdown" "md"} extension) (read-markdown f)
                    (= extension "org") (read-org f)
                    (= extension "html") (read-html f)
                    (= extension "clj") (read-clj f)
                    :else (throw (Exception. "Unknown Extension.")))]
      (cache/add f doc)
      doc)))

(defn dir-path [dir]
  {:pre [(#{:templates :public :site :posts} dir)]}
  (jio/file (:in-dir (config)) (str (name dir))))

(defn list-files [d]
  (-> (dir-path d)
      (FileUtils/listFiles (into-array ["markdown" "md" "clj" "org" "html"])
                           true)
      sort))

(defn read-template [template]
  (let [f (jio/file (dir-path :templates) template)]
    (or (cache/lookup f)
        (let [content (read-string (slurp-file f))]
          (cache/add f content)
          content))))

(defn write-out-dir [file content]
  (let [f (if (vector? file)
            (apply jio/file (:out-dir (config)) file)
            (jio/file (:out-dir (config)) file))]
   (FileUtils/writeStringToFile f content (:encoding (config)))))

(defn deploy-rsync [rsync out-dir host user deploy-dir post-deploy-cmd]
  (let [cmd (conj (vec rsync) out-dir (str user "@" host ":" deploy-dir))]
    (println "Running:" cmd)
    (log/info (:out (apply sh cmd)))
    (when post-deploy-cmd
      (log/info (:out (apply sh post-deploy-cmd))))))
