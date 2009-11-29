(ns simplenews.twitter
  (:use clojure.contrib.str-utils)
  (:import [winterwell.jtwitter Twitter]))

(def *tweet* (new Twitter "cubix_80" ""))

(def directs (ref {}))

(defn extract-url [text]
  (first (filter #(re-find #"^http://|https://" %) (re-split #"\s+" text))))

(defn extract-text [text]
  (str-join " " (filter #(not (re-find #"^(http://|https://)" %)) (re-split #"\s+" text))))

(defn save-directs []
  (dosync (ref-set directs (map (fn [dm] { (.getId dm) dm })
				(.getDirectMessages *tweet*)))))