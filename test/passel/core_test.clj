(ns passel.core-test
  (:require
   [clojure.test :refer :all]
   [passel.time :as time]
   [passel.template :as template]
   [passel.random :as random]
   [passel.log :as log]
   [passel.json :as json]
   [passel.error :as error]
   [passel.config :as config]
   [passel.case :as case]
   [passel.db :as db]))

(deftest ^:unit basic-test
  (log/init!)
  (log/info :testing)
  (is (= 2 2)))
