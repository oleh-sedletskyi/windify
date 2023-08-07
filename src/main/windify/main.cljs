(ns windify.main
  (:require [reagent.dom :as reagent-dom]
            [reagent.core :as reagent]
            [tick.core :as t]
            [clojure.string :as str]))

(defonce state (reagent/atom {}))

(defonce parsed-state (reagent/atom {}))

(defn get-api-url []
  ;; 50.512082226364534, 30.508248275600494
  ;; Europe/Kyiv
  (let [base-url "https://api.open-meteo.com/v1/forecast?"
        api-params {:latitude 50.5120
                    :longitude 30.5082
                    :hourly (->> ["temperature_2m"
                                  "relativehumidity_2m",
                                  "apparent_temperature"
                                  "precipitation"
                                  "rain"
                                  "showers"
                                  "snowfall"
                                  "snow_depth"
                                  "surface_pressure"
                                  "cloudcover"
                                  "windspeed_10m"
                                  "winddirection_10m"
                                  "windgusts_10m"]
                                 (str/join ","))
                    :daily (->> ["sunrise"
                                 "sunset"]
                                (str/join ","))
                    :windspeed_unit "ms"
                    :timezone "Europe%2FKyiv"
                    :forecast_days 7}
        params (->> api-params
                    (map (fn [[k v]]
                           (str (name k) "=" v)))
                    (str/join "&"))]
    (str base-url params)))

(defn parse-response []
  (let [daily (:daily @state)
        daily-vals [:time :sunrise :sunset]
        daily-data (->> ((apply juxt daily-vals) daily)
                        (apply interleave)
                        (partition (count daily-vals))
                        (map #(zipmap daily-vals %))
                        (group-by :time)
                        (map (fn [[k v]]
                               {k (first v)}))
                        (into {}))
        hourly-vals (:hourly @state)
        values [:windspeed_10m :winddirection_10m :snow_depth :surface_pressure :time :relativehumidity_2m :windgusts_10m :cloudcover :showers :apparent_temperature :precipitation :snowfall :temperature_2m :rain]]
    (->> ((apply juxt values) hourly-vals)
         (apply interleave)
         (partition (count values))
         (map #(zipmap values %))
         (group-by #(->> (t/date-time (:time %))
                         (t/date)
                         str))
         (map (fn [[k v]]
                {k {:sunset (get-in daily-data [k :sunset])
                    :sunrise (get-in daily-data [k :sunrise]) 
                    :hours (->> v
                                (sort-by #(t/date-time (:time %))))}}))
         (apply merge))))

(defn handler [response]
  (reset! state response)
  (reset! parsed-state (parse-response)))

(defn error-handler [err]
  (.log js/console (str "Error: " (.-message err))))

(defn get-data-from-api []
  (-> (js/fetch (get-api-url))
      (.then #(.json %))
      (.then #(js->clj % :keywordize-keys true))
      (.then #(handler %))
    (.catch #(error-handler %))))

(get-data-from-api)

(defn get-hourly-unit [unit]
  ;; (.log js/console (str unit))
  (get-in @state [:hourly_units unit]))

(defn get-value [unit data]
  (unit data))

(defn get-int-value [unit data]
  (js/parseInt (get-value unit data)))

(defn get-arrow [angle]
  (let [el (.createElement js/document "textarea")]
    (set! (.-innerHTML el) "&darr;")
    [:div
     {:style
      {:position :static
       :text-align :center
       :transform (str "rotate(" angle "deg)")}} (.-value el)]))

(defn get-hour-unit-style [date]
  (let [curr-dt (-> (t/now)
                    (t/offset-by 3) ;; TODO: Check https://juxt.github.io/tick/#_time_zones_offset
                    )
        date (t/date-time date)]
    (when (= (t/date date) (t/date curr-dt))
      (let [curr-time (-> curr-dt
                          (t/time))
            curr-hour (t/hour curr-time)
            hour (t/hour date)]
        (when (= hour curr-hour)
          {:style {:font-weight "bold"}})))))

;; TODO: Add cloudcover https://emojiguide.org/sun-behind-cloudelelelefdf
;; TODO: Add forecast days dropdown
;; TODO: Add units column

(defn app []
  [:div
   [:h1 "Windify"]

   [:div
    [:pre [:code
           ;; EEST
           ;; (t/in (t/instant) "UTC+02:00")
           ;; (get-hour-unit-style "2023-08-07T18:00")
           [:br]
           #_(-> (t/now)
                 (t/offset-by 3)
                 (t/time)
                 (t/minute))
           [:br]
           #_(let [daily (:daily @state)
                   values [:time :sunrise :sunset]
                   daily-data (->> ((apply juxt values) daily)
                                   (apply interleave)
                                   (partition (count values))
                                   (map #(zipmap values %))
                                   (group-by :time)
                                   (map (fn [[k v]]
                                          {(str k) (first v)}))
                                   (into {}))]
             ;; daily-data
               (get daily-data "2023-08-05"))]]]

   [:div
    {:style {:display :flex}}
    (for [[d v] @parsed-state]
      ^{:key d}
      [:div
       {:style {:flex "1"
                :padding "0.2em"
                :text-align :center
                :border "1px solid gray"}}
       ;; (.log js/console d v)
       [:p
        (-> (t/date d)
            t/day-of-week
            str
            str/capitalize
            (subs 0 2))
        " "
        (-> (t/date d)
            str)]
       [:div :Sunrise " " (-> (t/date-time (:sunrise v))
                              (t/time)
                              str)]
       [:div :Sunset " " (-> (t/date-time (:sunset v))
                             (t/time)
                             str)]

       [:div
        {:style {:display :flex}}
        (for [hour (:hours v)]
          ^{:key (:time hour)}
          [:div
           {:style {:flex "1"
                    :padding "0.1em"}}
           [:p (get-hour-unit-style (:time hour))
            (-> (t/date-time (:time hour))
                (t/hour)
                str)]
           [:div (get-hour-unit-style (:time hour)) (get-value :precipitation hour)]
           [:div (get-hour-unit-style (:time hour)) (get-int-value :apparent_temperature hour)]
           [:div (get-hour-unit-style (:time hour)) (get-int-value :windspeed_10m hour)]
           [:div (get-hour-unit-style (:time hour)) (get-int-value :windgusts_10m hour)]
           [:div (get-hour-unit-style (:time hour)) (get-arrow (get-value :winddirection_10m hour))]
           [:div (get-hour-unit-style (:time hour)) (get-int-value :cloudcover hour)]])]])]

   (comment
     (for [key (keys @state)]
       ^{:key (str key)} [:li key])
     [:div [:p "hourly:"]
      (for [key (keys (:hourly @state))]
        ^{:key (str key)} [:li key])]
     [:div
      [:pre {:style {:white-space "pre-wrap"}} [:code (let [k (-> (keys @parsed-state) first)]
                                                        (str {k (get @parsed-state k)}))]]])

   (comment
     [:div
      [:pre [:code (get-api-url)]]])])

(defn mount! []
  (reagent-dom/render [app]
                  (.getElementById js/document "app")))

(defn main! []
  (mount!)
  (.log js/console "main! started"))

(defn reload! []
  (mount!)
  (.log js/console "relaod!"))
