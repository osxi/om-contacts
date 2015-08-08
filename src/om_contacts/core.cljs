(ns ^:figwheel-always om-contacts.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [cljs.core.async :refer [put! chan <!]]
              [clojure.data :as data]
              [clojure.string :as string]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console...? what.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state
  (atom {:contacts [{:first "Zach" :middle-initial "A" :last "Ngo" :email "osxi@users.noreply.github.com"}
                    {:first "Bob" :middle "William" :last "Smith" :email "brunch_time@gmail.com"}
                    {:first "Jane" :last "Doe" :email "noreply@doe.io"}]}))

(defn middle-name [{:keys [middle middle-initial]}]
  (cond
    middle (str " " middle)
    middle-initial (str " " middle-initial ".")))

(defn display-name [{:keys [first last] :as contact}]
  (str last ", " first (middle-name contact)))

(defn contact-view [contact owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/li #js {:className "contact"}
              (dom/span nil (display-name contact))
              (dom/button #js {:onClick (fn [e] (put! delete @contact))} "Delete")))))

(defn contacts-view [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:delete (chan)})
    om/IWillMount
    (will-mount [_]
      (let [delete (om/get-state owner :delete)]
        (go (loop []
              (let [contact (<! delete)]
                (om/transact! data :contacts
                              (fn [xs] (vec (remove #(= contact %) xs))))
                (recur))))))
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/div nil
               (dom/h2 nil "Contacts List")
               (apply dom/ul #js {:className "contactsList"}
                      (om/build-all contact-view (:contacts data)
                                    {:init-state {:delete delete}}))))))

(defn parse-contact [contact-str]
  (let [[first middle last :as parts] (string/split contact-str #"\s+")
        [first last middle] (if (nil? last) [first middle] [first last middle])
        middle (when middle (string/replace middle "." ""))
        c (if middle (count middle) 0)]
    (when (>= (count parts) 2)
      (cond-> {:first first :last last}
        (== c 1) (assoc :middle-initial middle)
        (>= c 2) (assoc :middle middle)))))

(defn stripe [text bgc]
  (let [st #js {:backgroundColor bgc}]
    (dom/li #js {:style st} text)))

(om/root contacts-view app-state
         {:target (. js/document (getElementById "contacts"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (js/console.log "on-js-reload called"))
