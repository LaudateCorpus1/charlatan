(ns charlatan.core-test
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.test :refer :all]
            [org.senatehouse.expect-call :refer [expect-call]]
            [charlatan.core :refer :all]
            [charlatan.mountebank :as mb]))

(def mb-port 2525)
(def port 8080)

(deftest simple-response-to-ping
  (testing "mountebank simple response stub"
    (let [m (mb/start {:port mb-port})]
      (try
        (mb/create-imposter mb-port port
                            {:stubs [{:responses [{:is {:statusCode 202
                                                        :body "pong"}}]
                                      :predicates [{:equals {:method "GET"
                                                             :path "/ping"}}]}]})
        (let [response (http/get (str "http://localhost:" port "/ping")
                                 {:throw-exceptions false})]
          (is (= (:status response) 202))
          (is (= (:body response) "pong"))
          )
        (finally
          (mb/stop m))))))

(deftest test-with-approach
  (testing "mountebank simple response stub inside with-mb macro"
    (mb/with-mb {:port 2567}
      (mb/create-imposter port  {:stubs [{:responses [{:is {:statusCode 202
                                                           :body "pong"}}]
                                         :predicates [{:equals {:method "GET"
                                                                :path "/ping"}}]}]})
      (let [{status :status body :body} (http/get (str "http://localhost:" port "/ping")
                                                  {:throw-exceptions false})]
        (is (= status 202))
        (is (= body "pong"))))))

(deftest get-config
  (testing "get config works"
    (mb/with-mb {:port 2525}
      (let [{status :status body :body} (mb/get-config)]
        (is (= status 200))
        (is (= (-> body :options :port) 2525))))))

(deftest get-imposter
  (testing "get imposter works"
    (mb/with-mb {:port 2525 :debug true}
      (mb/create-imposter port {:stubs [{:responses [{:is {:statusCode 200
                                                           :body "pong"}}]
                                         :predicates [{:equals {:method "GET"
                                                                :path "/ping"}}]}]})
      (let [{status :status body :body} (http/get (str "http://localhost:" port "/ping")
                                                  {:throw-exceptions false})]
        (is (= status 200))
        (is (= body "pong")))
      (let [{status :status body :body} (mb/get-imposter port)
            match (-> body :stubs (first) :matches (first) :request)]
        (is (= (select-keys match [:method :path]) {:method "GET" :path "/ping"}))))))

(deftest with-running-mb-test
  (testing "correct referenced URL is used"
    (let [mb-url "http://mountebank"]
      (mb/with-running-mb {:url mb-url :port 4444}
        (is (= "http://mountebank:4444" (mb/mburl+))))))
  (testing "correct literal URL is used"
    (mb/with-running-mb {:url "http://mountebank" :port 4444}
      (is (= "http://mountebank:4444" (mb/mburl+))))))

(deftest with-imposter-test
  (testing "call delete-imposter on success"
    (expect-call [(mb/create-imposter [80 :imposter])
                  (mb/delete-imposter [80])]
      (mb/with-imposter 80 :imposter)))
  (testing "call delete-imposter on failure"
    (expect-call [(mb/create-imposter [80 :imposter])
                  (mb/delete-imposter [80])]
      (try
        (mb/with-imposter 80 :imposter
          (throw (Exception. "test")))
        (catch Exception _)))))
