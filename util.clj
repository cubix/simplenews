(ns simplenews.util
  (:require [clojure.contrib.str-utils2 :as s])
  (:import [java.io StringBufferInputStream])
  (:import [org.owasp.validator.html Policy AntiSamy CleanResults]))

(def *input-policy* (Policy/getInstance "/Users/cubix/code/antisamy-slashdot-1.3.xml"))
(def *antisamy* (new AntiSamy))

(defn clean-input [input-str]
  (if (not= nil input-str)
    (.getCleanHTML 
     (. *antisamy* scan input-str *input-policy*))))

(defn is-blank? [str-in]
  (or (nil? str-in) (re-find #"^\s*$" str-in)))

(defn nil-if-blank [str]
  (if (is-blank? str) nil str))

(defn clean-map [m]
  (or (apply merge 
	     (map (fn [[k v]]
		    { k (if (instance? String v) (nil-if-blank (clean-input v)) v)}) m)) 
      {}))

(defn the-str
  "Returns the name or string representation of x"
  [x]
  (if (instance? clojure.lang.Named x)
    (name x)
    (str x)))

(defn str-to-kws [rec]
  (zipmap (map keyword (keys rec))
          (vals rec)))


(defn mod-map [m kfn vfn]
  (or (apply merge 
	     (map (fn [[k v]]
		    { (kfn k) (vfn v) }) m)) 
      {}))

(defn mod-map-key [m kfn]
  (partial mod-map kfn))

(defn select-vals [a-map keys] 
  (into {} 
	(map #(if (%1 a-map) {%1 (%1 a-map)})
	     keys)))

(defn no-nil-vals [ & maps]
  (apply (partial merge-with #(or (nil-if-blank %2) (nil-if-blank %1))) maps))


(defn filter-map-vals [p m]
  (into {}
	(filter (comp p second) m)))

(def filter-nil-vals (partial filter-map-vals (comp not nil?)))


(defn pluralize-noun [noun num & [special case]]
  (cond 
    (= case num)
     special
    (= 1 num)
     (str num " " noun)
    :else 
     (str num " " noun "s")))

(defn time-now []
  (quot (.getTime (new java.util.Date)) 1000))

(defn time-since [ts]
  (let [time-diff (- (time-now) ts)]
    (cond (> time-diff  86400)
	   (let [days (java.lang.Math/round (/ time-diff 86400.0))]
	     (pluralize-noun "day" days))
	  (> time-diff 3599)
	   (let [hours (quot time-diff 3600)]
	     (pluralize-noun "hour" hours))
	  :else
	   (let [minutes (quot time-diff 60)]
	     (pluralize-noun "minute" minutes )))))

(defn convert-nl-to-br [str-in]
  (apply str 
	 (map #(if (= \newline %) 
		 "<br/>" %) 
	      str-in)))

(defn convert-br-to-nl [str-in]
  (s/replace str-in #"<\s*(:?br|BR)\s*\/{0,1}\s*>(:?\n\s*){0,1}" "\n"))
      