(ns simplenews.mongo
  (:use simplenews.util)
  (:use clojure.contrib.json.read)
  (:import (com.mongodb Mongo DBCollection BasicDBObject DB
                       DBObject DBCursor DBAddress)))

(defmulti mongo-query class)

(defmethod mongo-query String [criteria]
  (com.mongodb.util.JSON/parse criteria))

(defmethod mongo-query :default [criteria]
  (let [query (new com.mongodb.BasicDBObject)]
    (doseq [[field val] criteria]
      (.put query (the-str field) val))
    query))

(defn mongo-dbobj-to-map [db-obj]
  "Converts BasicDBObject to a clojure map."
  (binding [*json-keyword-keys* true]
    (let [json-str (.toString db-obj)
	  obj-map  (read-json json-str)]
      (apply merge
	     (map (fn [[k v]]
		    (if (instance? Number (.get db-obj k))
		      { k (.getLong db-obj k) }
		      { k v}))
		  obj-map)))))

(defn mongo-coll
  "Takes Mongo database url and collection name. Returns DBCollection
  object."
  [url coll-name]
  {:pre [(not (empty? url)) (not (empty? coll-name))] }
  (let [db-addr (new DBAddress url) 
	 mongo-conn (new Mongo db-addr)
	 db (.getDB mongo-conn (.getDBName db-addr))]
    (. db getCollection coll-name)))

(defn mongo-insert [coll rec-map]
  (let [rec (new com.mongodb.BasicDBObject)]
    (doseq [[k v] rec-map] (.put rec (the-str k) v))
    (.insert coll rec)))

(defn mongo-search [coll query-map]
  (let [query (mongo-query query-map)]
    (.find coll query)))  

(defn lazy-results [cursor]
  (lazy-seq
    (when-let [hasMore (.hasNext cursor)]
      (let [db-obj (.next cursor)]
	(if hasMore
	  (cons db-obj (lazy-results cursor))
	  (cons db-obj nil))))))

(def 
 #^{:doc "Converts lazy-seq of DBObjects to hash-maps."
    :arglists '([cursor])}
 map-results (comp (partial map 
			    mongo-dbobj-to-map)
		   lazy-results))

(defn drop-results [n cursor]
  (let [cursor (.skip cursor n)]
    (map-results cursor)))      

