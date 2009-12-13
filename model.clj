(ns simplenews.model
  (:use simplenews.config)
  (:use simplenews.util)
  (:use clojure.contrib.duck-streams))

;; Define structs

(defstruct news-item :id :timestamp :title :url :votes :body :children :parent :submitter :tag)
(defstruct user :username :password :karma :created :roles :email)

(defn classify-item [title url body]
    (cond  
      (and (not title) (not url) body) ::Comment
      (and title url (not body)) ::Url
      (and title (not url) body) ::Essay
      :else ::Unknown))

;; References for stateful data

(def item-list (ref { 0 (struct news-item 0 (time-now) *site-name* "/" 0 nil [] nil :cubix ::Url)}))
(def user-list (ref { :admin (struct user :admin "password" 0 (time-now) [:admin :moderator])}))
(def vote-list (ref {}))
(def item-counter (ref 0))

;; Abstract the storage since this will likely change

(defn find-item [id]
  (get @item-list id))

(defn find-user [user-id]
  (get @user-list user-id))

(defn find-votes [user-id]
  (get @vote-list user-id))

(defn get-user-list []
  @user-list)

;; Queries

(defn voted? [user item-id]
  (get (find-votes user) item-id))

(defn valid-user? [username password]
  (= (:password (find-user username)) password))

(defn submitted-before? [url]
  (first (filter (fn [[id item]] 
		   (= url (:url item))) 
		 (get-user-list))))

(defn user-owns-item? [item user-id]
  (= (:submitter item) user-id))

(defn deleted? [item]
  (= (:tag item) ::Deleted))

(defn not-frozen? [item]
  (and (not (deleted? item))
       (<= (- (time-now) (:timestamp item)) 1200))) ; 20 mins

(defn has-children? [item]
  (:children item))

;; Stateless modifications to individual items

(defn add-child [parent child]
  (assoc-in parent [:children]
	    (vec (cons (:id child) 
		       (:children parent)))))

(defn mod-prop [obj prop op]
  (assoc-in obj [prop] (op (prop obj))))

(defn add-vote [votes item-id]
  (assoc-in votes [item-id] 1))

;; Alter functions

(defn update-user-list [user]
  (alter user-list assoc-in [(:username user)] user))

(defn update-item-list [item]
  (alter item-list assoc-in [(:id item)] item))

(defn update-vote-list [user item-id]
  (let [new-votes (add-vote (find-votes user) item-id)]
    (alter vote-list assoc-in [user] new-votes)))

(defn self-vote [user-id item-id]
  (update-vote-list user-id item-id))

;; Make stateful changes and enforce relationships

(defn create-item [title url body parent-id user]
  (dosync (alter item-counter inc)
	  (struct news-item @item-counter (time-now) title url
		  1 body nil parent-id user (classify-item title url body))))

(defn do-vote [item-id dir vote-user]
  (dosync 
   (let [item (find-item item-id)
	 user (find-user (:submitter item))]
     (if (and item user ;(not= vote-user (:submitter item))
	      (not (voted? vote-user item-id)))
       (let [new-item (mod-prop item :votes dir)
	     new-user (mod-prop user :karma dir)]
	 (update-item-list new-item)
	 (update-user-list new-user)
	 (update-vote-list vote-user item-id))))))

; kind of dangerous
(defn edit-item [item]
  (dosync 
   (update-item-list item)))

(defn add-user [username password & [email]]
  (let [user-key (keyword username)]
    (dosync 
     (if (nil? (find-user user-key))
       (alter user-list assoc-in [user-key] 
	      (struct-map user 
		:username user-key
		:password password :karma 0 
		:created (time-now)
		:roles nil
		:email email))))))

(defn submit [item]
;  (let [item (clean-map item)]
  (dosync 
   (if (get @user-list (:submitter item))
     (if (and (not= nil (:url item)) (submitted-before? (:url item)))
       (do-vote (:id item) inc (:submitter item))
       (do (update-item-list item)
	   (if (find-item (:parent item))
	     (update-item-list (add-child (find-item (:parent item)) item)))
	   (self-vote (:submitter item) (:id item))))))) ;)

(defn remove-child [parent-item child-id]
  (mod-prop parent-item :children  
	    (fn [children] 
	      (vec (filter #(not= % child-id) 
			   children)))))

(defn hard-delete-item [item-id]
  (dosync
   (let [item (find-item item-id)
	 parent-item (find-item (:parent item))]
     (when (not (has-children? item))
       (ref-set item-list (dissoc @item-list item-id item))
       (update-item-list (remove-child parent-item item-id))))))

;; Stateless operations on data

(defn calc-rank [votes timestamp]
  (/ (- votes 1)
     (Math/pow (+ (/ (- (time-now)
			timestamp) 3600.0) 2) 1.5)))

(defn rank [lst]
    (map (fn [item]
	   (assoc-in item [:rank] 
		     (calc-rank (:votes item)
				(:timestamp item))))
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
     (->> (rank items)
	  (sort-by :rank rev-comp)
	  (enumerate))))

(defn bf-trav 
  "Does a breadth-first traversal of the item tree marking each node
  with the level. fn-acc is the accumalator function and fn-node-op is
  performed on each node."
  [item start fn-acc fn-node-op]
  (loop [queue [item] result start]
    (let [node  (first queue)
	  new-queue (rest queue)
	  level  (inc (or (:level node) 0))]
      (if (empty? queue)
      result
      (recur (concat (order-items 
		      (map #(assoc-in (find-item %) [:level]  level)
			   (:children node)))
		     new-queue)
	     (fn-acc result (fn-node-op node)))))))

(defn count-children [item]
  (bf-trav (assoc-in item [:level] 0) 0 + #(count (:children %))))

