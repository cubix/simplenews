(ns simplenews.twitter
  (:use simplenews.util)
  (:use clojure.contrib.str-utils)
  (:import [winterwell.jtwitter Twitter]))

(def *tweet* (new Twitter "cubix_80" ""))

(def directs (ref {}))

(def interval 60)

(defn date-seconds-ago [interval]
  (new java.util.Date (* (- (time-now) interval) 1000))) 


(defn extract-url [text]
  (first (filter #(re-find #"(\s)*((ht|f)tp(s?)://|mailto:)[\p{L}\p{N}]+[~\p{L}\p{N}\p{Zs}\-_\.@#$%&amp;;:,\?=/\+!]*(\s)*" %)
		 (re-split #"\s+" text))))

(def url-regex #"(\s)*((ht|f)tp(s?)://|mailto:)[\p{L}\p{N}]+[~\p{L}\p{N}\p{Zs}\-_\.@#$%&amp;;:,\?=/\+!]*(\s)*")

(defn match-url [token]
  (if-not (empty? token) 
	  (first (re-find url-regex 
			  token))))

	    
(defn sep-url 
  "Takes tweet text and seprate the URL from the text while removing
   RT, @username tokens and extra spaces."
  [txt]
  (loop [tokens (remove #(or (= \@ (first %))
				  (= "RT" %))
			(re-split #"\s+" txt))
	 url nil 
	 text nil]
    (let [token (first tokens)
	  remaining (rest tokens) 
	  found-url (match-url token)]
      (if (empty?  tokens)
	{ :url url 
	 :text (str-join " " text) }
	(if found-url
	  (recur  remaining found-url text)
	  (recur  remaining url (cons token text)))))))
	      

(defn save-directs []
  (dosync (ref-set directs (map (fn [dm] { (.getId dm) dm })
				(.getDirectMessages *tweet*)))))