(ns simplenews.view
    (:use simplenews.util)
    (:use simplenews.model)
    (:use compojure))
    
(alias 'model 'simplenews.model)

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
  (html
   (doctype :html4)
    [:html
      [:head
       (styles 'simplenews )
       ;[:script {:src "http://www.google.com/jsapi"}]
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
	[:div.outerbox page]]]))

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


(defn show-comment-field [item]
  (html
   (hidden-field "body")
   [:div#editcommentlbl (label "comment-lbl" "Comment")]
   [:div#editcomment (text-area "comment-in" (convert-br-to-nl (item :body)))]))

(defn show-title-field [item]
     [:div#edittitle (label "title-lbl" "Title")
      (text-field "title" (item :title)) ])

(defn show-url-field [item]
   [:div#editurl (label "url-lbl" "URL")
    (text-field "url" (item :url))])

(defmulti show-edit-fields :tag)

(defmethod show-edit-fields ::model/Comment [item]    
  (show-comment-field item))

(defmethod show-edit-fields ::model/Essay [item]
  (html
   (show-title-field item)
   (show-comment-field item)))

(defmethod show-edit-fields ::model/Url [item]
  (html
   (show-title-field item)
   [:div#editand [:p "and"]]
   (show-url-field item)))

(defn show-edit-submit [item]
  [:div#editbutton [:input {:type "submit"
			    :name "submitcomment"
			    :value "update"
			    :onClick (str "document.getElementById('body').value = "
					  "escapeVal(document.getElementById('comment-in').value)")}]])

(defn show-edit-form [item]
  (html 
   (form-to [:post (str "/edit/" (if (:id item) (:id item) 0))]
     (hidden-field "parent-id" (:parent item))
     (show-edit-fields item)
     (show-edit-submit item))))

(defn show-comment-form [item-id user & [edit]]
  (let [item (find-item item-id)]
    (html
      (form-to [:post (str (if edit "/edit/" "/comment/") item-id )] 
	(hidden-field "parent-id" (:parent item))
	(hidden-field "comment")
	[:div (text-area "comment-in" (if edit (:body item) ""))]
	[:div [:input {:type "submit"
		       :name "submitcomment"
		       :value (if edit "update" "add")
		       :onClick (str "document.getElementById('comment').value = "
				     "escapeVal(document.getElementById('comment-in').value)")}]]))))

(defn show-user-form []
  (html
     [:div#newuserform
      (form-to [:post (str "/new-user")]
	[:div (label "username-lbl" "Username") (text-field "username") ]
	[:div (label "password-lbl" "Password") (password-field "password") ]
	[:div (label "confirm-lbl" "Confirm") (password-field "confirm") ]
	[:div (submit-button "submit")])]))

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

(defn indent-class
  "Determines div class for indentation level."
  [item]
  (let [level (if (nil? (:level item)) 0 (:level item))]
    (if (> level 10) 
      "levelmax"
      (str "level" level))))

(defn at-top? [item]
  (= (:level item) 0))


(defn gen-vote-buttons [item]
   (html 
    [:div.up [:a {:href (str "/up-vote/" (:id item)) :id (str "up_" (:id item))} 
	       [:img.updown {:src "/images/uparrow.gif" :alt "up"}]]]
    [:div.down [:a {:href (str "/down-vote/" (:id item)) :id (str "down_" (:id item)) } 
	       [:img.updown {:src "/images/downarrow.gif" :alt "down"}]]]))


(defn gen-comment-bar [item auth user-id]
  (html 
   [:div.header 
    [:span.votes (when (and (not (deleted? item)) (not (voted? user-id (:id item))))
		  (gen-vote-buttons item)) ]
    [:span.points (:votes item) " points by " (name (:submitter item)) " " (time-since (:timestamp item)) " ago |"]
    (when (at-top? item)
      [:span.parent [:a {:href (str "/item/" (:parent item))} "parent"]])
    (when (not (at-top? item))
      [:span.reply  [:a {:href (str "/item/" (:id item))} "reply"]])
    (when (and (user-owns-item? item user-id) (not-frozen? item))
      [:span.edit [:a {:href (str "/edit/" (:id item))} "edit"]])
    (when (and (not (deleted? item)) 
	       (or (and (user-owns-item? item user-id) 
			(not-frozen? item)) 
		   (moderator? user-id)))
      [:span.delete [:a {:href (str "/delete/" (:id item))} "delete"]])]))


(defn show-item [auth user-id item]
  (html [:div {:class (indent-class item)}
	 (gen-comment-bar item auth user-id)
	 (when (or (= ::model/Url (:tag item)) (= ::model/Essay (:tag item)))
	   [:p.itemtitle [:a {:href (:url item)} (:title item) ]])
	 (if (deleted? item) [:p.deleted "[deleted]"] (:body item))
	 (when  (at-top? item)
	   (show-comment-form (:id item) (:submitter item)))])) 

(defn derive-url [item]
  (if (or (nil? (:url item)) (= "" (:url item)))
    (str "/item/" (:id item))
    (:url item)))
   
(defn gen-title-link [item]
  [:a {:href (derive-url item)} [:p.front-title-text (:title item) ]])

(defn show-front [items]
  (html (map (fn [item]
	       [:div.frontitem
		[:span.enum (str (:enumeration item) ".")]
		[:span.votebuttons (gen-vote-buttons item)]
		[:span.fronttitle (gen-title-link item)]
		[:span.statusline (gen-status-line item)]])
	     items)))


(defn bf-format [auth user node-id]
  (bf-trav (assoc-in (find-item node-id) [:level] 0)
	   "" str (partial show-item auth user)))
