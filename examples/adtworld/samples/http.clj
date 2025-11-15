(ns adtworld.samples.http
  "業務アプリ寄りの例: Web API のレスポンス種別を ADT で扱い、Ring 形式へ落とし込む。"
  (:require [app.adtworld :as adt]))

(def HttpResult
  (adt/adt {:name :HttpResult
            :constructors
            {:Ok {:fields [:body]}
             :Created {:fields [:location :body]}
             :Redirect {:fields [:location]}
             :ClientError {:fields [:status :message :details]}
             :ServerError {:fields [:status :message :info]}}}))

(defn ok [body] (adt/value HttpResult :Ok {:body body}))
(defn created [location body] (adt/value HttpResult :Created {:location location :body body}))
(defn redirect [location] (adt/value HttpResult :Redirect {:location location}))
(defn client-error
  ([status message] (client-error status message nil))
  ([status message details]
   (adt/value HttpResult :ClientError {:status status :message message :details details})))
(defn server-error
  ([message] (server-error message nil))
  ([message info]
   (adt/value HttpResult :ServerError {:status 500 :message message :info info})))

(defn ->ring-response
  "HttpResult を Ring レスポンスマップに変換する。"
  [result]
  (adt/match result
    {:Ok (fn [{:keys [body]}]
           {:status 200
            :headers {"content-type" "application/json"}
            :body body})
     :Created (fn [{:keys [location body]}]
                {:status 201
                 :headers {"location" location
                           "content-type" "application/json"}
                 :body body})
     :Redirect (fn [{:keys [location]}]
                 {:status 302
                  :headers {"location" location}
                  :body ""})
     :ClientError (fn [{:keys [status message details]}]
                    {:status status
                     :headers {"content-type" "application/json"}
                     :body {:error message
                            :details details}})
     :ServerError (fn [{:keys [status message info]}]
                    {:status status
                     :headers {"content-type" "application/json"}
                     :body {:error message
                            :info info}})}))

(defn fetch-user [user-db id]
  (if-let [user (get user-db id)]
    (ok {:user user})
    (client-error 404 "ユーザーが見つかりません" {:id id})))

(defn create-user [user-db {:keys [id email]}]
  (cond
    (get user-db id)
    (client-error 409 "ID が衝突しています" {:id id})

    (not (re-find #"@" email))
    (client-error 422 "メールアドレスの形式が不正です" {:email email})

    :else
    (created (str "/users/" id)
             {:user/id id
              :user/email email})))

(defn handler
  "極小サンプルの HTTP ハンドラ。"
  [{:keys [uri request-method] :as request}]
  (let [db {1 {:id 1 :email "alice@example.com"}}
        result (case [request-method uri]
                 [:get "/users/1"] (fetch-user db 1)
                 [:post "/users"] (create-user db {:id 2 :email "bob@example.com"})
                 (client-error 404 "ルートがありません" {:uri uri
                                                  :method request-method
                                                  :params (:params request)}))]
    (->ring-response result)))

(defn -main [& _]
  (doseq [req [{:request-method :get :uri "/users/1"}
               {:request-method :get :uri "/users/42"}
               {:request-method :post :uri "/users"}]]
    (println "----")
    (println "request:" req)
    (println "response:" (handler req))))

(comment
  (handler {:request-method :get :uri "/users/1"})
  ;; => {:status 200, :body {:user {:id 1, ...}}, ...}

  (handler {:request-method :get :uri "/users/99"})
  ;; => {:status 404, :body {:error "ユーザーが見つかりません", ...}}

  (handler {:request-method :post :uri "/users"})
  ;; => {:status 201, :headers {"location" "/users/2", ...}}
  )
