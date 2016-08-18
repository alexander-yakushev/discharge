(ns discharge.org
  (:require [discharge.config :refer [config]]
            [discharge.utils :refer :all]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :as jio]
            [clojure.string :as s]
            [hiccup.core :as hiccup])
  (:import java.io.File
           org.apache.commons.io.FileUtils))

(def ^:private cut-line "#\\+readmore\n")
(def ^:private safe-cut-line "#-readmore\n")

(defn- extract-synopsis [content]
  (let [cut-pos (.indexOf content safe-cut-line)
        cut-len (count safe-cut-line)]
    (if (> cut-pos -1)
      ;; We found a cut line
      (let [synopsis (subs content 0 cut-pos)]
        [synopsis (str synopsis (subs content (+ cut-pos cut-len)))])
      ;; There is no cut line, try grabbing first paragraph
      (if (not= (.indexOf content "<p>")
                (.lastIndexOf content "<p>"))
        ;; There is more than one paragraph, use first as synopsis
        [(subs content (.indexOf content "<p>") (.indexOf content "</p>"))
         content]
        [nil content]))))

(defn- pygmentize-code [content]
  (let [matcher (re-matcher #"(?is)#\+begin_src ([^\s]+)\n(.+?)#\+end_src" content)
        new-content (StringBuilder.)]
    (loop [[_ lang code] (re-find matcher)
           previous-end 0]
      (if code
        (let [match-end (.end matcher)
              pygmentized (:out (sh "pygmentize" "-l" lang "-f" "html"
                                    "-O" (str "cssclass=highlight source-" lang)
                                    :in (s/trim code)))]
          (.append new-content
                   (subs content previous-end (.start matcher)))
          (.append new-content
                   (format "#+begin_html\n%s\n#+end_html\n"
                           (if-not (empty? pygmentized) pygmentized
                                   (format "<pre>COULD NOT PYGMENTIZE: %s\n%s</pre>"
                                           lang code))))
          (recur (re-find matcher) match-end))
        (do (.append new-content (subs content previous-end))
            (str new-content))))))

(defn- pre-org-transform [content]
  (let [matcher (re-matcher #"(?s)#\+begin_hiccup\n(.+?)#\+end_hiccup" content)
        new-content (StringBuilder.)]
    (loop [code (re-find matcher)
           previous-end 0]
      (if code
        (let [match-end (.end matcher)
              tmp-file (File/createTempFile "discharge-code" ".txt")
              ]
          (.append new-content
                   (subs content previous-end (.start matcher)))
          (.append new-content
                   (format "#+begin_html\n%s\n#+end_html\n"
                           (binding [*ns* (find-ns 'discharge.core)]
                             (hiccup/html (eval (read-string (second code)))))))
          (recur (re-find matcher) match-end))
        (do (.append new-content (subs content previous-end))
            (str new-content))))))

(defn- post-org-transform [content]
  (let [matcher (re-matcher #"(?s)\{\{(.+?)\}\}" content)
        new-content (StringBuilder.)]
    (loop [[code-with-limiters code] (re-find matcher)
           previous-end 0]
      (if code
        (let [match-end (.end matcher)
              evaled (binding [*ns* (find-ns 'discharge.core)]
                       (try
                         (hiccup/html (eval (read-string code)))
                         (catch Exception ex code-with-limiters)))]
          (.append new-content
                   (subs content previous-end (.start matcher)))
          (.append new-content evaled)
          (recur (re-find matcher) match-end))
        (do (.append new-content (subs content previous-end))
            (str new-content))))))

(defn- run-emacs-org [file]
  (sh (:emacs (config))
      "-q" "-batch" "-eval"
      (str
       "(progn "
       (apply str (:emacs-eval (config)))
       " (find-file \"" (.getAbsolutePath file) "\") "
       " (princ (org-no-properties (org-export-as 'html nil nil t nil))))")))

(defn read-org-file [f]
  (when-not (:emacs (config))
    (fail "Path to Emacs is required for org files."))
  (let [tmp-file (File/createTempFile "discharge-" ".org")]
    (-> (slurp f :encoding (:encoding (config)))
        (.replaceAll cut-line safe-cut-line)
        pre-org-transform
        pygmentize-code
        (->> (spit tmp-file)))
    (let [content (post-org-transform (:out (run-emacs-org tmp-file)))]
      (.delete tmp-file)
      (extract-synopsis content))))

;; New posts

(def ^:private org-file-template
  "#+title: %s
#+tags:
#+OPTIONS: toc:nil author:nil
#+post-type: news
")

(defn new-post
  "Create new post with the current date and the given title."
  [title]
  (let [fname (str (format-date (java.util.Date.) "YYYY-MM-dd-")
                   (->> (take 4 title)
                        (map (fn [word]
                               (.toLowerCase (.replaceAll word "[^\\w\\d]" ""))))
                        (interpose "-")
                        (apply str))
                   ".org")
        f (jio/file (:blog-dir (config)) fname)]
    (FileUtils/writeStringToFile
     f
     (format org-file-template (s/join (interpose " " title)))
     (:encoding (config)))
    (sh "ec" (str f))))
