(ns adtworld.samples.workflow
  "ヘルプデスクのチケット状態を ADT で表現し、遷移ロジックをデータ駆動で書く例。"
  (:require [app.adtworld :as adt]))

(def Ticket
  (adt/data
    [:Ticket "サポートチケットのライフサイクル。"
     [:Draft :id :summary :reporter]
     [:Assigned :id :summary :reporter :assignee]
     [:WaitingOnCustomer :id :summary :reporter :assignee :requested-at]
     [:Resolved :id :summary :reporter :assignee :resolution]
     [:Closed :id :summary :reporter :assignee :resolution :closed-at]
     [:Cancelled :id :summary :reporter :reason]]))

(defn draft [id summary reporter]
  (adt/value Ticket :Draft {:id id :summary summary :reporter reporter}))

(defn- forbid [ticket message]
  (throw (ex-info message {:state (adt/ctor ticket)
                           :ticket (adt/fields ticket)})))

(defn assign
  "Draft -> Assigned のみ許可。"
  [ticket assignee]
  (adt/match ticket
    {:Draft (fn [{:keys [id summary reporter]}]
              (adt/value Ticket :Assigned {:id id :summary summary :reporter reporter :assignee assignee}))
     :else (fn [_] (forbid ticket "すでに担当者が決まっています。"))}))

(defn request-customer
  "Assigned -> WaitingOnCustomer。顧客にアクションを依頼した時間を記録。"
  [ticket instant]
  (adt/match ticket
    {:Assigned (fn [{:keys [id summary reporter assignee]}]
                 (adt/value Ticket :WaitingOnCustomer {:id id
                                                       :summary summary
                                                       :reporter reporter
                                                       :assignee assignee
                                                       :requested-at instant}))
     :else (fn [_] (forbid ticket "顧客確認は担当者アサイン後のみ送信できます。"))}))

(defn resolve-ticket
  "Assigned / WaitingOnCustomer -> Resolved。"
  [ticket resolution]
  (adt/match ticket
    {:Assigned (fn [{:keys [id summary reporter assignee]}]
                 (adt/value Ticket :Resolved {:id id
                                              :summary summary
                                              :reporter reporter
                                              :assignee assignee
                                              :resolution resolution}))
     :WaitingOnCustomer (fn [{:keys [id summary reporter assignee]}]
                          (adt/value Ticket :Resolved {:id id
                                                       :summary summary
                                                       :reporter reporter
                                                       :assignee assignee
                                                       :resolution resolution}))
     :else (fn [_] (forbid ticket "現在の状態からは解決済みにできません。"))}))

(defn close-ticket
  "Resolved -> Closed に遷移し、クローズ日時を刻む。"
  [ticket instant]
  (adt/match ticket
    {:Resolved (fn [{:keys [id summary reporter assignee resolution]}]
                 (adt/value Ticket :Closed {:id id
                                            :summary summary
                                            :reporter reporter
                                            :assignee assignee
                                            :resolution resolution
                                            :closed-at instant}))
     :else (fn [_] (forbid ticket "解決済みになっていないチケットはクローズできません。"))}))

(defn cancel
  "Draft / Assigned / WaitingOnCustomer -> Cancelled。"
  [ticket reason]
  (adt/match ticket
    {:Draft (fn [{:keys [id summary reporter]}]
              (adt/value Ticket :Cancelled {:id id :summary summary :reporter reporter :reason reason}))
     :Assigned (fn [{:keys [id summary reporter]}]
                 (adt/value Ticket :Cancelled {:id id :summary summary :reporter reporter :reason reason}))
     :WaitingOnCustomer (fn [{:keys [id summary reporter]}]
                          (adt/value Ticket :Cancelled {:id id :summary summary :reporter reporter :reason reason}))
     :else (fn [_] (forbid ticket "この状態からはキャンセルに移行できません。"))}))

(defn reopen
  "Closed -> Assigned。"
  [ticket assignee]
  (adt/match ticket
    {:Closed (fn [{:keys [id summary reporter]}]
               (adt/value Ticket :Assigned {:id id
                                            :summary summary
                                            :reporter reporter
                                            :assignee assignee}))
     :else (fn [_] (forbid ticket "クローズ済みのチケットのみ再オープン可能です。"))}))

(defn state-label
  "UI 表示に使える簡単な説明文字列。"
  [ticket]
  (adt/match ticket
    {:Draft "未着手"
     :Assigned (fn [{:keys [assignee]}] (str "対応中 (" assignee ")"))
     :WaitingOnCustomer "顧客回答待ち"
     :Resolved "解決済み (未クローズ)"
     :Closed (fn [{:keys [closed-at]}] (str "クローズ済み (" closed-at ")"))
     :Cancelled "キャンセル"}))

(defn -main [& _]
  (let [now (java.time.Instant/now)
        t0 (draft "T-42" "VPN が切れる" "alice@example.com")
        t1 (assign t0 "bob")
        t2 (request-customer t1 now)
        t3 (resolve-ticket t2 {:note "VPN クライアントを再インストール"})
        t4 (close-ticket t3 now)]
    (doseq [ticket [t0 t1 t2 t3 t4]]
      (println (format "%s -> %s"
                       (name (adt/ctor ticket))
                       (state-label ticket))))))

(comment
  (def t0 (draft "T-42" "VPN が切れる" "alice@example.com"))
  (def t1 (assign t0 "bob"))
  (def t2 (request-customer t1 #inst "2025-02-14T03:00:00.000-00:00"))
  (def t3 (resolve-ticket t2 {:note "VPN クライアント再インストール"}))
  (def t4 (close-ticket t3 #inst "2025-02-15T09:30:00.000-00:00"))
  (state-label t4)
  ;; => "クローズ済み (2025-02-15T09:30:00.000Z)"

  (try
    (assign t4 "carol")
    (catch clojure.lang.ExceptionInfo ex
      (ex-data ex)))
  ;; => {:state :Closed, :ticket {...}}
  )
