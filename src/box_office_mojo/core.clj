(ns box-office-mojo.core
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [net.cgrand.enlive-html :as html]))

(def ^:dynamic *base-url* "http://boxofficemojo.com")

(def ^:dynamic *gross-selector*
  #{[:table (html/attr= :cellpadding "5")
     [:tr (html/but html/first-child)]]})

(def ^:dynamic *release-selector*
  #{[:b
     [(html/attr-contains :href "/movies/?id=")]]})

(def ^:dynamic *release-date-selector*
  #{[(html/text-pred #(re-find #"\) - \d" %))]})

(def ^:dynamic *custom-date-formatter* (f/formatter "yyyyMMdd"))

(defn- nth-child [i node]
  (first (html/select [node] #{[html/root :> (html/nth-child i)]})))

(defn- str->number [s]
  (when-let [t (apply str (filter #(Character/isDigit %) s))]
    (Integer/parseInt t)))

(defn- fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn- gross-by-date-url [date]
  (str *base-url* "/daily/chart/?view=1day&sortdate=" (f/unparse *custom-date-formatter* date) "&p=.htm"))

(defn- grosses [date]
  (html/select (fetch-url (gross-by-date-url date)) *gross-selector*))

(defn- extract-gross [node]
  (let [title         (nth-child 3 node)
        day           (nth-child 11 node)
        theaters      (nth-child 8 node)
        gross         (nth-child 5 node)
        gross-to-date (nth-child 10 node)]
    {:title (html/text title)
     :day (str->number (html/text day))
     :theaters (str->number (html/text theaters))
     :gross (str->number (html/text gross))
     :gross-to-date (str->number (html/text gross-to-date))}))

(defn grosses-by-date [date]
  (map extract-gross (grosses date)))

(defn- releases-url [year]
  (str *base-url* "/schedule/?view=bydate&release=theatrical&yr=" year "&p=.htm"))

(defn- releases [year]
  (html/select (fetch-url (releases-url year)) *release-date-selector*))

(defn- extract-release [year [title-node release-node]]
  (let [title      (html/text title-node)
        date-parts (map str->number
                        (rest (re-find #"(\d+)\/(\d+)"
                                       (html/text release-node))))]
    {:title title
     :release-date (apply t/date-time
                          (concat [year] date-parts))}))

(defn upcoming-releases [year]
  (let [html    (fetch-url (releases-url year))
        titles  (html/select html *release-selector*)
        dates   (html/select html *release-date-selector*)
        results (map vector titles dates)]
    (map (partial extract-release year) results)))
