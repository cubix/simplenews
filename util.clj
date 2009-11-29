(ns simplenews.util
  (:import [java.io StringBufferInputStream])
  (:import [org.owasp.validator.html Policy AntiSamy CleanResults]))

(def *input-policy* (Policy/getInstance "antisamy-slashdot-1.3.xml"))
(def *antisamy* (new AntiSamy))

(defn clean-input [input-str]
  (if (not= nil input-str)
    (.getCleanHTML 
     (.scan *antisamy* input-str *input-policy*))))

(defn is-blank? [str-in]
  (or (nil? str-in) (re-find #"^\s*$" str-in)))

(defn clean-map [m]
  (or (apply merge 
	     (map (fn [[k v]]
		    { k (clean-input v) }) m)) 
      {}))

(defn mod-map [m kfn vfn]
  (or (apply merge 
	     (map (fn [[k v]]
		    { (kfn k) (vfn v) }) m)) 
      {}))

(defn mod-map-key [m kfn]
  (partial mod-map kfn))

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