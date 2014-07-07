(ns discharge.core
  (:require [discharge
             [io :as io :refer [list-files]]
             [org :as org]
             [cache :as cache]
             [config :refer [config]]
             [utils :refer :all]]
            [clojure-watch.core :as watcher]
            [clojure.java.io :as jio]
            [clojure.tools.logging :as log]
            [clojure.tools.cli :refer [cli]]
            [clojure.java.browse :refer [browse-url]]
            [hiccup.core :as hiccup]
            [hiccup page util]
            [ring.adapter.jetty :refer [run-jetty]]
            [cemerick.url :as url])
  (:use [ring.middleware.file]
        [ring.util.response])
  (:import (java.net URL)
           (org.apache.commons.io FileUtils FilenameUtils))
  (:gen-class))

(defn setup-logging []
  (let [logger (java.util.logging.Logger/getLogger "")]
    (doseq [handler (.getHandlers logger)]
      (. handler setFormatter
         (proxy [java.util.logging.Formatter] []
           (format
             [record]
             (str "[+] " (.getLevel record) ": " (.getMessage record) "\n")))))))

(defmacro log-time-elapsed
  "Evaluates expr and logs the time it took.  Returns the value of expr."
  {:added "1.0"}
  [msg & expr]
  `(let [start# (. System (currentTimeMillis))
         ret# (do ~@expr)]
     (log/info ~msg "\t" (/ (double (- (. System (currentTimeMillis)) start#)) 1000.0) "secs")
     ret#))

(defn post-url
  "Given a post file return its URL."
  [file]
  (-> (.toURI (io/dir-path :posts))
      (.relativize (.toURI file))
      .getPath
      (FilenameUtils/removeExtension)
      (.split "-" 4)
      (->> (interleave (repeat \/))
           (apply str))
      (str "/")))

(defn site-url [f & [ext]]
  (-> (.toURI (io/dir-path :site))
      (.relativize (.toURI f))
      .getPath
      (FilenameUtils/removeExtension)
      (str "." (or ext (:default-extension (config))))))

(def ^:dynamic metadata nil)
(def ^:dynamic content nil)

(defn template [page]
  (let [[m c] page
        template (or (:template m)
                     (:default-template (config)))
        template-string (if (= template :none)
                          c
                          (io/read-template template))]
    (binding [*ns* (find-ns 'discharge.core)
              metadata m, content c]
      (apply str (hiccup/html {:mode :html} (hiccup.page/doctype :html5)
                              (eval template-string))))))

(defn process-site
  "Process site pages."
  []
  (doseq [f (io/list-files :site)]
    (try
     (let [[metadata content] (io/read-doc f)]
       (when (empty? content)
         (log/warn "Empty content in" (.getAbsolutePath f)))
       (io/write-out-dir
        (site-url f (:extension metadata))
        (template [(assoc metadata :type :site) content])))
     (catch Exception ex (log/error (.getMessage ex))))))

;; Create tags page.

(defn tag-map
  "Create a map of tags and posts contining them. {tag1 => [file1 file2..]}"
  []
  (reduce
   (fn [h v]
     (try
       (let [[metadata] (io/read-doc v)
             tags (:tags metadata)]
         (reduce
          (fn [m p]
            (let [[tag info] p]
              (if (nil? (m tag))
                (assoc m tag [info])
                (assoc m tag (conj (m tag) v)))))
          h (partition 2 (interleave tags (repeat v)))))
       (catch Exception e
         (log/error "Failed to read file" v)
         h)))
   (sorted-map)
   (filter #(not (nil? (:tags (first (try (io/read-doc %) (catch Exception e nil))))))
           (io/list-files :posts))))

(defn create-tags
  "Create and write tags page."
  []
  (io/write-out-dir
   "tags/index.html"
   (template
    [{:title "Tags", :template (:default-template (config)),
      :type :site}
     (hiccup/html
      [:ul
       (for [[tag posts] (tag-map)]
         [:h4 {:id tag} tag
          [:ul (for [post posts
                     :let [[metadata] (io/read-doc post)]]
                 [:li [:a {:href (post-url post)} (:title metadata)]])]])])])))

;; Create pages for latest posts.

;; Create RSS Feed.

(defn post-xml
  "Create RSS item node."
  [file]
  (let [[metadata content] (io/read-doc file)]
    [:item
     [:title (hiccup.util/escape-html (:title metadata))]
     [:link  (str (URL. (URL. (:site-url (config))) (post-url file)))]
     [:description (hiccup.util/escape-html content)]]))

(defn create-rss
  "Create RSS feed."
  ([]
     (create-rss (take 10 (reverse (io/list-files :posts))) nil)
     (doseq [[tag posts] (tag-map)
             :when (or (= (:rss-for-tags (config)) :all)
                       (some (partial = tag) (:rss-for-tags (config))))]
       (create-rss posts tag)))
  ([posts tag]
     (let [in-dir (io/dir-path :posts)
           {:keys [blog-url site-url site-title]} (config)
           rel-url (->> [(when blog-url
                           (subs blog-url (inc (count site-url))))
                         (when tag "tags") (when tag tag)
                         "rss-feed"]
                        (remove nil?))]
       (io/write-out-dir
        (apply jio/file rel-url)
        (hiccup/html (hiccup.page/xml-declaration "UTF-8")
                     (hiccup.page/doctype :xhtml-strict)
                     [:rss {:version "2.0"
                            :xmlns:atom "http://www.w3.org/2005/Atom"}
                      [:channel
                       [:title (hiccup.util/escape-html (if tag
                                                          (str site-title " - " tag)
                                                          site-title))]
                       [:link (or blog-url site-url)]
                       [:atom:link {:href (apply url/url (or blog-url site-url) rel-url)
                                    :rel "self"
                                    :type "application/rss+xml"}]
                       [:description
                        (hiccup.util/escape-html (:site-description (config)))]
                       (pmap post-xml posts)]])))))

(defn create-sitemap
  "Create sitemap."
  []
  (io/write-out-dir
   "sitemap.xml"
   (let [base (:site-url (config))]
     (hiccup/html (hiccup.page/xml-declaration "UTF-8")
                  [:urlset {:xmlns "http://www.sitemaps.org/schemas/sitemap/0.9"}
                   [:url [:loc base]]
                   (map #(vector :url [:loc (str base %)])
                        (map post-url (io/list-files :posts)))
                   (map #(vector :url [:loc (str base "/" %)])
                        (map site-url (io/list-files :site)))]))))

(defn pager
  "Return previous, next navigation links."
  [page max-index posts-per-page]
  (let [count-total (count (io/list-files :posts))
        older [:div {:class "pager-left"}
               [:a {:href (str "/latest-posts/" (- page 1) "/")}
                "&laquo; Older Entries"]]
        newer [:div {:class "pager-right"}
               [:a {:href (str "/latest-posts/" (+ page 1) "/")}
                "Newer Entries &raquo;"]]]
    (cond
     (< count-total posts-per-page) nil
     (= page max-index) (list older)
     (= page 0) (list newer)
     :default (list older newer))))

(defn snippet
  "Render a post for display in index pages."
  [f]
  (let [[metadata content] (io/read-doc f)]
    [:div [:h2 [:a {:href (post-url f)} (:title metadata)]]
     [:p {:class "publish_date"}
      (format-date (re-find #"\d*-\d*-\d*"
                            (FilenameUtils/getBaseName (str f)))
                   "yyyy-MM-dd" "dd MMM yyyy")]
     [:p content]]))

(defn create-latest-posts
  "Create and write latest post pages."
  []
  (let [posts-per-page (:posts-per-page (config))
        posts (partition posts-per-page
                         posts-per-page
                         []
                         (reverse (io/list-files :posts)))
        pages (partition 2 (interleave (reverse posts) (range)))
        [_ max-index] (last pages)]
    (doseq [[posts page] pages]
      (io/write-out-dir
       (str "latest-posts/" page "/index.html")
       (template
        [{:title (:site-title (config))
          :description (:site-description (config))
          :template (:default-template (config))}
         (hiccup/html (list (map #(snippet %) posts) (pager page max-index posts-per-page)))])))))

;; Create Archive Pages.

(defn posts-by-month
  "Returns a map {month => [post1 post2]} starting with most recent."
  []
  (->> (io/list-files :posts)
       (group-by #(re-find #"\d*-\d*" (FilenameUtils/getBaseName (str %))))
       (sort-by first)
       reverse))

(defn create-archives
  "Create and write archive pages."
  []
  (io/write-out-dir
   "archives.html"
   (template
    [{:title "Archives" :template (:default-template (config))
      :type :site}
     (hiccup/html
      [:ul
       (for [[date posts] (posts-by-month)]
         [:h4 (format-date date "yyyy-MM" "MMMM yyyy")
          [:ul (for [f posts
                     :when (.exists f)]
                 [:li [:a {:href (post-url f)}
                       (:title (first (io/read-doc f)))]])]])])])))

(defn create-aliases
  "Create redirect pages."
  ([]
     (dorun (map create-aliases (io/list-files :posts)))
     (dorun (map create-aliases (io/list-files :site))))
  ([file]
     (let [doc (io/read-doc file)]
       (when-let [aliases (-> doc first :alias)]
         (doseq [alias (read-string aliases)]
           (io/write-out-dir
            alias
            (hiccup/html [:html
                          [:head
                           [:meta {:http-equiv "content-type" :content "text/html; charset=utf-8"}]
                           [:meta {:http-equiv "refresh" :content (str "0;url=" (post-url file))}]]])))))))

(defn process-posts
  "Create and write post pages."
  []
  (->> (io/list-files :posts)
       (pmap (fn [f]
               (try
                 (let [[metadata content] (io/read-doc f)
                       url (post-url f)]
                   (when (empty? content)
                     (log/warn "Empty content in" f))

                   (io/write-out-dir
                    (jio/file (subs url 1) "index.html")
                    (template
                     [(assoc metadata :type :post, :url url) content])))
                 (catch Exception e (log/error (.getMessage e))))))
       dorun))

(defn process-public
  "Copy public from in-dir to out-dir."
  []
  (let [in-dir (io/dir-path :public)
        out-dir (jio/file (:out-dir (config)))]
    (doseq [f (.list in-dir)
            :let [f (jio/file in-dir f)]]
      (try
        (if (.exists f)
          (if (.isFile f)
            (FileUtils/copyFileToDirectory f out-dir)
            (FileUtils/copyDirectoryToDirectory f out-dir)))
        (catch Exception ex (log/error (.getMessage ex)))))))

(defn create
  "Build Site."
  []
  (doto (jio/file (:out-dir (config)))
    FileUtils/deleteDirectory
    .mkdir)

  (log-time-elapsed "Processing Public" (process-public))

  (when (seq (.list (io/dir-path :posts)))
    (log-time-elapsed "Processing Posts" (process-posts))
    (log-time-elapsed "Creating RSS\t" (create-rss))
    (when (:create-tags (config))
      (log-time-elapsed "Creating Tags" (create-tags)))
    (when (:create-archives (config))
      (log-time-elapsed "Creating Archives " (create-archives)))
    (log-time-elapsed "Creating Sitemap" (create-sitemap))
    (when (:create-aliases (config))
      (log-time-elapsed "Creating Aliases" (create-aliases)))
    (when (:blog-as-index (config))
      (log-time-elapsed "Creating Latest Posts" (create-latest-posts))
      (let [max (apply max (map read-string (.list (jio/file (:out-dir (config))
                                                             "latest-posts"))))]
        (FileUtils/copyFile
         (jio/file (:out-dir (config)) "latest-posts" max "index.html")
         (jio/file (:out-dir (config)) "index.html")))))

  (log-time-elapsed "Processing Site" (process-site)))

(defn serve-discharge [req]
  (let [mime-types {".clj" "text/plain"
                    ".mp4" "video/mp4"
                    ".ogv" "video/ogg"}]
    (if-let [f (file-response (:uri req) {:root (:out-dir (config))})]
      (if-let [mimetype (mime-types (re-find #"\..+$" (:uri req)))]
        (merge f {:headers {"Content-Type" mimetype}})
        f))))

(defn watch-and-rebuild
  "Watch for changes and rebuild site on change."
  []
  (watcher/start-watch
   [{:path (:root-dir (config))
     :event-types [:create :modify :delete]
     :options {:recursive true}
     :bootstrap (fn [_]
                  (create)
                  (future (run-jetty serve-discharge {:port 8080}))
                  (browse-url "http://127.0.0.1:8080"))
     :callback (fn [ev fname]
                 (let [f (jio/file fname)]
                   (when (and (or (FilenameUtils/directoryContains
                                   (:in-dir (config)) fname)
                                  (= f @discharge.config/config-file))
                              (not (.isHidden f)))
                     (try
                       (log/info "Rebuilding site..." (.getName f))
                       (cache/drop f)
                       (create)
                       (catch Exception ex (log/error "Rebuild failed:" (.getMessage ex)))))))}]))

(defn -main [& args]
  (let [[opts [& trailing] banner]
        (cli args
             ["--dir" "Manually specify the path to blog folder." :default false :flag false]
             ["--build" "Build site." :default false :flag true]
             ["--tmp" "Use tmp location override :out-dir" :default false :flag true]
             ["--jetty" "View site." :default false :flag true]
             ["--watch" "Watch site and rebuild on change." :default false :flag true]
             ["--rsync" "Deploy site." :default false :flag true]
             ["--help" "Show help" :default false :flag true]
             ["--new" "Create new post" :default false :flag true])
        {:keys [dir build tmp jetty watch rsync help new]} opts]

    (when help
      (println "Discharge")
      (println banner)
      (System/exit 0))

    (setup-logging)

    (let [config-file (jio/file (or dir ".") "config.clj")]
      (if (.exists config-file)
        (discharge.config/set-config-file! config-file)
        (fail (str "Cannot find configuration file: " config-file))))

    (cond build (log-time-elapsed "Total build time" (create))
          watch (watch-and-rebuild)
          jetty (do (future (run-jetty serve-discharge {:port 8080}))
                    (browse-url "http://127.0.0.1:8080"))
          rsync (let [{:keys [rsync out-dir host user deploy-dir post-deploy-cmd]} (config)]
                  (io/deploy-rsync rsync out-dir host user deploy-dir post-deploy-cmd))
          new (org/new-post trailing)
          :default (println "Use --help for options.")))
  (shutdown-agents))

#_(def serv (future (run-jetty serve-discharge {:port 8080})))

#_(do (discharge.cache/clear)
      (create))
