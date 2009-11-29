(ns simplenews.data
  (:use simplenews.config)
  (:use clojure.contrib.duck-streams)
  (:use simplenews.model))

(defn save-state-files [dir]
  (dosync (spit (str dir "/item-list.clj") @item-list)
	  (spit (str dir "/user-list.clj") @user-list)
	  (spit (str dir "/vote-list.clj") @vote-list)
	  (spit (str dir "/item-counter.clj") @item-counter)))

(defn load-state-files [dir]
  (dosync (ref-set item-list (load-file (str dir "/item-list.clj")))
	  (ref-set vote-list (load-file (str dir "/vote-list.clj")))
	  (ref-set item-counter (load-file (str dir "/item-counter.clj")))
	  (ref-set user-list (load-file (str dir "/user-list.clj")))))

(defn backup-agent [[dir sleep-time]]
  (save-state-files dir)
  (send-off (agent [dir sleep-time]) backup-agent)
  (Thread/sleep sleep-time))

(backup-agent [*data-dir* *sleep-time*])
