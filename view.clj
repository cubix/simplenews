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
	page]]))

(defn show-submit-form [item & edit]
  (html
   (form-to [:post (str "/comment/" (if (:id item) (:id item) 0))]
     (hidden-field "parent-id" (:parent item))
     (if (or (not edit)  (:url item))
       (html
	[:div#submittitle (label "title-lbl" "Title") (text-field "title" (if edit (:title item))) ]
	[:div#submitand [:p "and"]]
	[:div#submiturl (label "url-lbl" "URL") (text-field "url" (if edit (:url item)))]))
     (if (or (not edit) (not (:url item)))
       (html 
	(when (not edit) [:div#submitor [:p "or"]])
	[:div#submitcommentlbl (label "comment-lbl" "Comment")]
	[:div#submitcomment (text-area "comment" (if edit (:body item)))]))
     [:div#submitbutton (submit-button (if edit "update" "add"))])))

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


(defmulti show-edit-fields :tag)

(defn show-comment-field [item]
  (html
     [:div#editcommentlbl (label "comment-lbl" "Comment")]
     [:div#editcomment (text-area "body" (item :body))]))

(defn show-title-field [item]
     [:div#edittitle (label "title-lbl" "Title")
      (text-field "title" (item :title)) ])

(defn show-url-field [item]
   [:div#editurl (label "url-lbl" "URL")
    (text-field "url" (item :url))])

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


(defn show-edit-form [item]
  (html 
   (form-to [:post (str "/edit/" (if (:id item) (:id item) 0))]
     (hidden-field "parent-id" (:parent item))
     (show-edit-fields item)
     [:div#editbutton (submit-button "update")])))

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
		      :onClick "document.getElementById('comment').value = escapeVal(document.getElementById('comment-in').value)"}]]))))

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
   [:table
    [:tr [:td [:a {:href (str "/up-vote/" (:id item)) } 
	       [:img.updown {:src "/images/uparrow.gif"}]]]] 
    [:tr [:td [:a {:href (str "/down-vote/" (:id item)) }
	       [:img.updown {:src "/images/downarrow.gif"}]]]]]))

(defn gen-comment-bar [item auth user-id]
  (html 
   [:div.header 
    [:table
     [:tr 
      [:td (when (not (voted? user-id (:id item)))
	    (gen-vote-buttons item)) ]
      [:td (:votes item) " points by " (name (:submitter item)) " " (time-since (:timestamp item)) " ago"] 
      [:td (when (at-top? item)
	     (html "| "  [:a {:href (str "/item/" (:parent item))} "parent"])) ]
      [:td (when (not (at-top? item))
	     (html "| " [:a {:href (str "/item/" (:id item))} "reply"]))]
      [:td (when (and (user-owns-item? item user-id) (not-frozen? item))
	     (html "| " [:a {:href (str "/edit/" (:id item))} "edit"]
	     " | " [:a {:href (str "/delete/" (:id item))} "delete"]))]]]]))

(defn show-item [auth user-id item]
  (html [:div {:class (indent-class item)}
	 (gen-comment-bar item auth user-id)
	 (when (:url item)
	   [:p.itemtitle [:a {:href (:url item)} (:title item) ]])
	   [:p (if (= ::model/Deleted (:tag item)) "[DELETED]" (:body item))]
	 (when  (at-top? item)
	   (show-comment-form (:id item) (:submitter item)))])) 

;; (defn show-edit-form [item user auth]
;;   (show-page  
;;    (html 
;;     [:div#submittitle (text-area "title" (:title item))]
;;     (when (item :url) [:div#submiturl (text-area "url-edit-ta" (:url item))])
;;     (if (:body item)
;;       (html
;;        [:p (:body item)]
;;        (show-comment-form (:id item)
;; 			  user true))))))

(defn derive-url [item]
  (if (or (nil? (:url item)) (= "" (:url item)))
    (str "/item/" (:id item))
    (:url item)))

	

(defn show-front [items]
  (html [:table 
	 (map (fn [item]
		(html [:tr
		       [:td.enum (str (:enumeration item) ".")]
		       [:td
			[:div 
			 [:a {:href (str "/up-vote/" (:id item))} 
			  [:img.updown {:src "/images/uparrow.gif"}]]]
			[:div 
			 [:a {:href (str "/down-vote/" (:id item))}
			  [:img.updown {:src "/images/downarrow.gif"}]]] "\n"]
		       [:td.header [:a {:href (derive-url item)} [:p.front-title (:title item) ]]] "\n"]
		      [:tr
		       [:td]
		       [:td]
		       [:td.header
			(gen-status-line item)] "\n"]
		      [:tr])) items) ]))


(defn bf-format [auth user node-id]
  (bf-trav (assoc-in (find-item node-id) [:level] 0)
	   "" str (partial show-item auth user)))
