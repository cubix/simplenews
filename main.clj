(ns simplenews.main
  (:use simplenews.config)
  (:use simplenews.util)
  (:use simplenews.model)
  (:use simplenews.view)
  (:use simplenews.data)
  (:use compojure))

 

(declare *session* *param* *request* *username* *auth* *id* *user-key*)

(defn testweb []
  (html [:p "My username is:" *username* " and I'm auth'd: " *auth* " id: " *id* "; " *param*]))

(defmacro par-bind
  "Makes the session, params, headers and request hash globally available (via thread-specific bindings)."
  [& body]
  `(binding [*session* ~'session
             *param* ~'params
             *request* ~'request
	     *username* ~'(session :username)
	     *auth* ~'(session :authenticated)
	     *user-key* (if ~'(session :username) (keyword ~'(session :username)))
	     *id* (if ~'(params :id) (Integer/parseInt ~'(params :id)))]
     ~@body))

(defn do-comment []
  (if  (and (is-blank? (*param* :comment))
	    (is-blank? (*param* :url))
	    (is-blank? (*param* :title)))
    (redirect-to (str "/reply/" (*param* :parent-id)))
    (do (submit 
	 (create-item (*param* :title)
		      (*param* :url) 
		      (convert-nl-to-br (*param* :comment)) *id* *user-key*))
	(if (= "" (*param* :parent-id))
	  (redirect-to (str "/"))
	  (redirect-to (str "/item/" *id*))))))


(defn do-item []
  (if (find-item *id*)
    (show-page (bf-format *auth* *user-key* *id*) 
	       *auth* *user-key*)
    [404 "page not found"]))

(defn do-vote-cast [fdir]
  (if (do-vote *id* fdir *user-key*)
    (if (> (:parent (find-item *id*)) 0)
      [(redirect-to (format "/item/%d" *id*))]
      [(redirect-to (format "/" *id*))])
    "Error"))

(defn do-edit []
  (let [item (find-item *id*)]
    (if (and (= *user-key* (:submitter item))
	     (not= (:body item) (:comment *param*)))
      (edit-item (assoc-in item [:body] (:comment *param*))))
    (redirect-to (str "/item/" *id*))))

(defn do-front []
  (show-page (show-front) *auth* *user-key*))

(defn with-clean [& route-seq]
  (fn [request]
    (let [cleaned-params (clean-map (:params request))
	  request (assoc request :params cleaned-params)
	  handler (apply routes route-seq)]
      (handler request))))

(defn redirect-login [path]
  (ANY "*"
    [(session-assoc :nextpage path)
     (redirect-to "/login")]))

(defn with-auth [& route-seq]
  (fn [request]
    (let [session (:session request)
	  path    (:uri request)
	  handler (if (= (:authenticated session) 'true)
		    (apply routes route-seq)
		    (redirect-login path))]
      (handler request)))) 

(defroutes public-routes
  (GET "/item/0"
    (redirect-to "/"))
  (GET "/css/:style.css" 
    (serve-file *css-dir* (str (params :style) ".css")))
  (GET "/js/:script.js" 
    (serve-file *js-dir* (str (params :script) ".js")))
  (GET "/images/:image"
    (serve-file *image-dir* (params :image)))
  (GET "/item/:id"
    (par-bind (do-item)))
  (GET "/new-user"
    (show-page (show-user-form)))
  (GET "/submit-user"
    (let [user (params :username)
	  password (params :password)
	  confirm (params :confirm)]
      (if (and (> (count user) 2) (= password confirm) (> (count password) 5))
	(do (add-user user password)
	    (redirect-to "/"))
	"Error")))
  (GET "/"
    (par-bind (do-front))))
  
(defroutes sess-routes
  (GET "/login"
    (show-page (show-login-form)))
  (GET "/auth-user"
    (if (validate-user (keyword (params :username)) (params :password))
      [ (set-session (assoc (assoc session :authenticated 'true) 
		       :username (params :username))) 
	(redirect-to (if (nil? (session :nextpage)) "/" (session :nextpage)))]
      (show-page "login failed"))))
 
(defroutes auth-routes
  (GET "/test/:id"
    (par-bind (testweb)))
  (GET "/reply/:id"
    (par-bind
      (show-page (html (show-item *auth* *user-key* (find-item *id*))
		       (show-comment-form 
			*id*
			*user-key*)))))
  (GET "/edit/:id"
    (par-bind
      (show-edit-form (find-item *id*) *user-key* *auth*)))
  (POST "/edit/:id"
    (par-bind
      (do-edit)))
  
    (ANY "/comment/:id"
      (par-bind (do-comment)))
    (GET "/submit"
      (par-bind
	(show-page (show-submit-form (find-item *id*)) *auth* *user-key*)))
    (GET "/up-vote/:id"
      (par-bind (do-vote-cast inc)))
    (GET "/down-vote/:id"
      (par-bind (do-vote-cast dec)))
    (GET "/logout"
      (show-page (form-to [:post "/logout"] 
		   [:div [:p "Are you sure you want to logout?"]]
		   [:div (submit-button "logout")])))
    (POST "/logout"
      [(session-assoc :authenticated 'false)
       (redirect-to "/")]))

(decorate auth-routes  with-auth with-clean with-session)
;(decorate auth-routes  with-auth with-session)
(decorate sess-routes  with-clean with-session )
(decorate public-routes  with-session)

(defroutes all-routes
  public-routes
  sess-routes
  auth-routes
  (ANY "*"
    [404 "Page not found"]))

(run-server {:port *port*}
  "/*" (servlet all-routes))

