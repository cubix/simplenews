(ns simplenews.view
    (:use simplenews.util)
    (:use simplenews.model)
    (:use compojure)
    (:use [compojure.http.response :only (create-response)]))

    
(defn styles
  "Return HTML to include CSS stylesheets."
  [& styles]
  (for [style styles]
    (include-css (str "/css/" style ".css"))))

(defn scripts
  "Return HTML to include javascript files."
  [& scripts]
  (for [script scripts]
    (include-js (str "/js/" script ".js"))))

(defn show-page [page & [auth user-id]]
  (println "show-page: user-id" user-id)
  (html
   (doctype :html4)
    [:html
      [:head
       (styles 'simplenews )
       [:script {:src "http://www.google.com/jsapi"}]
       (scripts 'encode-form)
        ;(javascript-tag "SyntaxHighlighter.all({light: true});")
        [:title (:title (find-item 0))]]
      [:body 
       [:div.navigator 
	[:span.tab [:a {:href "/"} "Home"]] 
	[:span.tab [:a {:href "/submit"} "Submit"]]
	(if auth
	  (html [:span.tab [:a {:href "/logout"} "Logout"]]
		[:span.tab (str (name user-id) "(" (:karma (find-user user-id)) ")")]
		)
	  [:span.tab [:a {:href "/login"} "Login"]])]
	page]]))


(defn show-submit-form [item]
  (html
   (form-to [:post (str "/comment/" (if (:id item) (:id item) 0))]
     (hidden-field "parent-id" (:parent item))
     [:div#submittitle (label "title-lbl" "Title") (text-field "title") ]
     [:div#submitand [:p "and"]]
     [:div#submiturl (label "url-lbl" "URL") (text-field "url")]
     [:div#submitor [:p "or"]]
     [:div#submitcommentlbl (label "comment-lbl" "Comment")]
     [:div#submitcomment (text-area "comment")]
     [:div#submitbutton (submit-button "add")])))


(defn show-comment-form [item-id user & [edit]]
  (let [item (find-item item-id)]
    (html
     (form-to [:post (str (if edit "/edit/" "/comment/") item-id )]
       (hidden-field "parent-id" (:parent item))
       (hidden-field "comment")
       [:div (text-area "comment-input" (if edit (:body item) ""))]
       [:div [:input {:type "submit"
		      :name "submitcomment"
		      :value (if edit "update" "add")
		      :onClick "escapeVal(comment-input,'<br>'); comment.value = comment-input.value;"}]]))))

(defn show-user-form []
  (html
   (form-to [:get (str "/submit-user")]
     [:div (label "username-lbl" "Username") (text-field "username") ]
     [:div (label "password-lbl" "Password") (password-field "password") ]
     [:div (label "confirm-lbl" "Confirm") (password-field "confirm") ]
     [:div (submit-button "submit") ])))

(defn show-login-form []
  (html
   (form-to [:get (str "/auth-user")]
     [:div (label "username-lbl" "Username")
      (text-field "username")]
     [:div (label "password-lbl" "Password")
      (password-field "password")]
     [:div (submit-button "submit")])))


      
(defn gen-status-line [item]
  (let [comment-count (count-children item)]
    (html (name (:submitter item)) " | "
	  (time-since (:timestamp item)) " | " 
	  (pluralize-noun "point" (:votes item))
	  " | "
	  [:a {:href (str "/item/" (:id item))} 
	   (pluralize-noun "comment" comment-count "discuss" 0)])))

(defn show-item [item]
  (html [:div {:class (if (> (if (nil? (:level item)) 0 (:level item)) 10) "levelmax" (str "level" (:level item)))}
	 [:span.header (name (:submitter item)) " | " (:votes item) " | " 

	  [:a {:href (str "/up-vote/" (:id item)) } "up"] " | "
	  [:a {:href (str "/down-vote/" (:id item)) } "down"] " | " 
	  [:a {:href (str "/reply/" (:id item))} "reply"]]
	 (if (not (nil? (:title item)))
	   [:p.itemtitle [:a {:href (:url item)} (:title item) ]]
	   [:p (:body item)])])) 


(defn show-item [auth user-id item]
  (html [:div {:class (if (> (if (nil? (:level item)) 0 (:level item)) 10) "levelmax" (str "level" (:level item)))}
	 [:span.header (name (:submitter item)) " | " (:votes item) " | " 
	  (when (= (:level item) 0) (html [:a {:href (str "/item/" (:parent item))} "parent"] " | "))
	  [:a {:href (str "/up-vote/" (:id item)) } "up"] " | "
	  [:a {:href (str "/down-vote/" (:id item)) } "down"]
	  (when (not= (:level item) 0) 
	    (html " | " [:a {:href (str "/item/" (:id item))} "reply"]))
	  (when (and auth (= (:submitter item) user-id)) 
	    (html " | " [:a {:href (str "/edit/" (:id item))} "edit"]))]
	 (when (:url item)
	   [:p.itemtitle [:a {:href (:url item)} (:title item) ]])
	   [:p (:body item)]
	 (when  (= (:level item) 0) ;(nil? (:url item)))
	   (show-comment-form (:id item) (:submitter item)))
	 ])) 

(defn show-edit-form [item user auth]
  (show-page  
   (html 
    (when (:title item)
      [:div (text-area "title" (:title item))]
      (when (:url item)
	[:div (text-area "url" (:url item))]))
    (if (:body item)
      (html
       [:p (:body item)]
       (show-comment-form (:id item)
			  user true))))))

(defn show-front []
  (html [:table 
	 (let [items (order-items 
		      (map find-item (:children (find-item 0))))]
	   (map (fn [item]
		  (html [:tr
			 [:td.enum (str (:enumeration item) ".")]
			   [:td
			    [:div 
			     [:a {:href (str "/up-vote/" (:id item))} 
			      [:img.updown {:src "http://ycombinator.com/images/grayarrow.gif"}]]]
			    [:div 
			     [:a {:href (str "/down-vote/" (:id item))}
			      [:img.updown {:src "http://ycombinator.com/images/graydown.gif"}]]] "\n"]
			   [:td.itemtitle [:a {:href (:url item)} (:title item) ]] "\n"]
			[:tr
			 [:td]
			 [:td]
			 [:td.header
			  (gen-status-line item)] "\n"]
			[:tr])) items))]))

(defn bf-trav [queue result]
  (let [node  (first queue)
	new-queue (rest queue)
	level (:level node)]
    (if (empty? queue)
      result
      (bf-trav (concat (order-items (map #(assoc-in (find-item %) [:level] (inc level))
					 (:children node)))
		       new-queue)
	       (str result (show-item node) "\n")))))

(defn bf-trav [queue result fn-acc fn-node-op]
  (let [node  (first queue)
	new-queue (rest queue)
	level (:level node)]
    (if (empty? queue)
      result
      (bf-trav (concat (order-items 
			(map #(assoc-in (find-item %) [:level] (inc level))
			     (:children node)))
		       new-queue)
	       (fn-acc result (fn-node-op node))
	       fn-acc fn-node-op))))


(defn bf-format [node-id]
  (bf-trav [(assoc-in (find-item node-id) [:level] 0)] ""))

(defn bf-format [auth user node-id]
  (bf-trav [(assoc-in (find-item node-id) [:level] 0)] 
	   "" str (partial show-item auth user)))