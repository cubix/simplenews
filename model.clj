(ns simplenews.model
  (:use simplenews.config)
  (:use simplenews.util)
  (:use clojure.contrib.duck-streams))

(defstruct news-item :id :timestamp :title :url :votes :body :children :parent :submitter)
(defstruct user :username :password :karma :created)

(def item-list (ref { 0 (struct news-item 0 (time-now) *site-name* "/" 0 nil [] nil :cubix)}))
(def user-list (ref { :admin (struct user :admin "password" 0 0)}))
(def vote-list (ref {}))
(def item-counter (ref 0))

(defn create-item [title url body parent-id user]
  (dosync (alter item-counter inc)
	  (struct news-item @item-counter (time-now)
		  title
		  (if (empty? url)
		    (str "/item/" @item-counter) 
		    url)
		  1 body nil parent-id user)))


(defn add-child [parent child]
  (assoc-in parent [:children]
	    (cons (:id child) 
		  (vec (:children parent)))))

(defn mod-prop [obj prop op]
  (assoc-in obj [prop] (op (prop obj))))

(defn update-user-list [user]
  (alter user-list assoc-in [(:username user)] user))

(defn update-item-list [item]
  (alter item-list assoc-in [(:id item)] item))

(defn find-item [id]
  (get @item-list id))

(defn find-user [username]
  (get @user-list username))

(defn get-user-list []
  @user-list)

(defn voted? [user item-id]
  (get (get @vote-list user) item-id))

(defn add-vote [votes item-id]
  (assoc-in votes [item-id] 1))

(defn update-vote-list [user item-id]
  (let [new-votes (add-vote (get @vote-list user) item-id)]
    (alter vote-list assoc-in [user] new-votes)))

(defn do-vote [item-id dir vote-user]
  (dosync 
   (let [item (find-item item-id)
	 user (find-user (:submitter item))]
     (if (and item user (not= vote-user (:submitter item))
	      (not (voted? vote-user item-id)))
       (let [new-item (mod-prop item :votes dir)
	     new-user (mod-prop user :karma dir)]
	 (update-item-list new-item)
	 (update-user-list new-user)
	 (update-vote-list vote-user item-id))))))

(defn edit-item [item]
  (dosync 
   (update-item-list item)))

(defn calc-rank [votes timestamp]
  (/ (- votes 1)
     (Math/pow (+ (/ (- (time-now)
			timestamp) 3600.0) 2) 1.5)))

(defn rank [lst]
    (map (fn [item]
	   (assoc-in item [:rank] 
		     (calc-rank (:votes item) (:timestamp item))))
	 lst))

(defn enumerate [items]
  (map (fn [item enum] 
	 (assoc-in item [:enumeration] enum)) 
       items 
       (range 1 (inc (count items)))))

(defn order-items [items]
  (let [rev-comp (fn [x y] 
		   (cond (< x y) 1 
			 (= x y) 0 
			 (< y x) -1))]
     (enumerate
      (sort-by :rank rev-comp
	       (rank items)))))

(defn add-user [username password]
  (let [user-key (keyword username)]
    (dosync 
     (if (nil? (find-user user-key))
       (alter user-list assoc-in [user-key] 
	      (struct-map user 
		:username user-key
		:password password :karma 0 
		:created (time-now)))))))

(defn validate-user [username password]
  (= (:password (find-user username)) password))

(defn submitted-before? [url]
  (first (filter (fn [[id item]] (= url (:url item))) (get-user-list))))

(defn submit [item]
  (dosync 
   (if (get @user-list (:submitter item))
     (if (and (not= nil (:url item)) (submitted-before? (:url item)))
       (do-vote (:id item) inc (:submitter item))
       (do (update-item-list item)
	   (if (find-item (:parent item))
	     (update-item-list (add-child (find-item (:parent item)) item))))))))

(defn count-children [item]
  (loop [queue [item] result 0]
    (let [node (first queue)
	  new-queue (rest queue)]
      (if (empty? queue)
	result
	(recur (concat new-queue (map find-item (:children node)))
	       (+ result (count (:children node))))))))

