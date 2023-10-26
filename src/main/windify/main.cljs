(ns windify.main
  (:require [reagent.dom :as reagent-dom]
            [reagent.core :as reagent]
            [tick.core :as t]
            [clojure.string :as str]))

(defonce state (reagent/atom {}))

(defonce parsed-state (reagent/atom {}))

(defonce params
  (reagent/atom
   {:forecast-days 3
    :latitude 50.5120
    :longitude 30.5082}))

(defn get-api-url []
  ;; 50.512082226364534, 30.508248275600494
  ;; Europe/Kyiv
  (let [base-url "https://api.open-meteo.com/v1/forecast?"
        api-params {:latitude       (:latitude @params)
                    :longitude      (:longitude @params)
                    :hourly         (->> ["temperature_2m"
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
                    :daily          (->> ["sunrise"
                                          "sunset"]
                                         (str/join ","))
                    :windspeed_unit "ms"
                    :timezone       "Europe%2FKyiv"
                    :forecast_days  (:forecast-days @params)}
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
         (apply merge)
         (into (sorted-map)))))

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
  (get-in @state [:hourly_units unit]))

(defn get-value [unit data]
  (unit data))

(defn get-int-value [unit data]
  (js/parseInt (get-value unit data)))

(defn get-html-entity [html-code style]
  (let [el (.createElement js/document "textarea")]
    (set! (.-innerHTML el) html-code)
    [:div
     (merge {:style {:position :static
                     :text-align :center}}
            style) (.-value el)]))

(def emoji
  {:sun                    "&#9728;&#65039;"
   :sun-behind-small-cloud "&#127780;&#65039;"
   :sun-behind-cloud       "&#9925;"
   :sun-behind-rain-cloud  "&#127782;&#65039;"
   :sun-behind-large-cloud "&#127781;&#65039;"
   :cloud                  "&#9729;&#65039;"
   :cloud-with-rain        "&#127783;&#65039;"
   :cloud-with-snow        "&#127784;&#65039;"
   :thermometer            "&#127777;&#65039;"
   :north-arrow            "&darr;"})

(defn get-arrow [angle]
  (let [style {:style
               {:position :static
                :text-align :center
                :color (cond
                         (or
                          (> angle 315)
                          (< angle 46)) :blue
                         (and
                          (> angle 135)
                          (< angle 225)) :orangered
                         :else :black)
                :transform (str "rotate(" angle "deg)")}}]
    (get-html-entity (:north-arrow emoji) style)))

(defn get-sky-view [precipitation clouds]
  (let [emoji (cond
                (and
                 (= precipitation 0)
                 (< clouds 11)) (:sun emoji)
                (and
                 (= precipitation 0)
                 (< clouds 31)) (:sun-behind-small-cloud emoji)
                (and
                 (= precipitation 0)
                 (< clouds 61)) (:sun-behind-cloud emoji)
                (and
                 (= precipitation 0)
                 (< clouds 91)) (:sun-behind-large-cloud emoji)
                (and
                 (= precipitation 0)
                 (> clouds 90)) (:cloud emoji)
                (and
                 (> precipitation 0)
                 (< clouds 85)) (:sun-behind-rain-cloud emoji)
                (and
                 (> precipitation 0)
                 (> clouds 84)) (:cloud-with-rain emoji)
                :else "&#9940;")]
    (get-html-entity emoji {:style
                            {:position :static
                             :text-align :center}})))

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
        (cond
          (= hour curr-hour) {:style {:font-weight "bold"}}
          (< hour curr-hour) {:style {:opacity "0.6"}}
          :else nil)))))

(defn get-temperature-styling [time temp]
  (let [style (get-hour-unit-style time)]
    (assoc-in style [:style :color] (cond
                                      (> temp 27) :orangered
                                      (< temp -5) :blue
                                      :else :black))))

(defn get-wind-styling [time wind]
  (let [style (get-hour-unit-style time)]
    (assoc-in style [:style :color] (cond
                                      (< wind 4)  "#ffffff80"
                                      (> wind 15) :red
                                      (> wind 10) :orangered
                                      :else :black))))

(defn get-rain-styling [time rain]
  (let [style (get-hour-unit-style time)]
    (assoc-in style [:style :color] (cond
                                      (= rain 0) "#ffffff80"
                                      (> rain 4) :blue
                                      :else :black))))

;; TODO: Add flash message
;; TODO: Store location in cookies
;; TODO: Pick location from the map
;; TODO: Add coloring to temperature/wind/rain/wind-direction
;; TODO: Fix vertical formatting

(defn get-hidden-div []
  [:div {:style {:opacity "0"
                 ;; :padding "0.1em"
                 }} "Tu"])

(defn app []
  [:div
   [:h1 "Windify"]
   [:div
    [:label {:for :forecast-days} "Forecast for:"]
    [:select {:name :forecast-days
              :id :forecast-days
              :value (:forecast-days @params)
              :on-change (fn [e]
                           (swap! params assoc :forecast-days (-> e .-target .-value))
                           (get-data-from-api))}
     [:option {:value 3} "3 days"]
     [:option {:value 5} "5 days"]
     [:option {:value 7} "7 days"]
     [:option {:value 14} "14 days"]]]

   [:div
    [:label {:for :location} "Location:"]
    [:select {:name :location
              :id :location-dropdown
              :value (str (:latitude @params) ";" (:longitude @params))
              :on-change (fn [e]
                           (let [[lat long] (->> (-> e .-target .-value)
                                                 (#(str/split % #";"))
                                                 (map (fn [v]
                                                        (js/parseFloat v))))]
                             (swap! params assoc :latitude lat :longitude long)
                             (get-data-from-api)))}
     [:option {:value "50.4236;30.382"} "Наталія"]
     [:option {:value "50.512;30.5082"} "Олег"]
     [:option {:value "42.7128;27.7453"} "Сашко"]]]

   [:div
    [:pre [:code
           ;; (str @params)
           ;; EEST
           ;; (t/in (t/instant) "UTC+02:00")
           ;; (get-hour-unit-style "2023-08-07T18:00")
           ]]]

   [:div
    {:style {:float "left"}}
    (doall
     (for [[d v] @parsed-state]
       ^{:key d}
       [:div
        {:style {:flex "1"
                 :padding "0.2em"
                 :text-align :center
                 :border "0.5px solid gray"}}
        [:div
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
         [:div {:style {:flex "1"
                        :padding "0.1em"}}
          [:div :Hour]
          [:div (get-html-entity (:sun-behind-small-cloud emoji) {})]
          [:div "Rain," (get-hourly-unit :precipitation)]
          [:div "T," (get-hourly-unit :apparent_temperature)]
          [:div "Wind," (get-hourly-unit :windspeed_10m)]
          [:div "Gusts," (get-hourly-unit :windgusts_10m)]
          [:div (get-hourly-unit :winddirection_10m)]
          [:div "Clouds," (get-hourly-unit :cloudcover)]]

         (for [hour (:hours v)]
           ^{:key (:time hour)}
           (let [time (:time hour)
                 precipitation (get-value :precipitation hour)
                 cloudcover (get-int-value :cloudcover hour)
                 temperature (get-int-value :apparent_temperature hour)
                 windspeed (get-int-value :windspeed_10m hour)
                 windgusts (get-int-value :windgusts_10m hour)
                 winddirection (get-value :winddirection_10m hour)]
             [:div
              {:style {:flex "1"
                       :padding "0.1em"}}
              [:div (get-hour-unit-style time)
               (-> (t/date-time time)
                   (t/hour)
                   str)]
              (get-sky-view precipitation cloudcover)
              [:div (get-rain-styling time precipitation) precipitation]
              [:div (get-temperature-styling time temperature) temperature]
              [:div (get-wind-styling time windspeed) windspeed]
              [:div (get-wind-styling time windgusts) windgusts]
              [:div (get-hour-unit-style time) (get-arrow winddirection)]
              [:div (get-hour-unit-style time) cloudcover]]))]]))]

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
