(ns ruler.rules-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [ruler.rules :as rules]))

(deftest check-rule-test

  (testing "Invalid rule key"
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type String :wut String})) "Invalid rule key"))

  (testing "Valid rules"
    (is (nil? (rules/check-rule {:key :name :type String})) "Only required fields")
    (is (nil? (rules/check-rule {:key :name :type String :req false})) "With optional field: req")
    (is (nil? (rules/check-rule {:key :name :type String :req true})) "With optional field: req")
    (is (nil? (rules/check-rule {:key :name :type String :req-depends []})) "With optional field: req-depends")
    (is (nil? (rules/check-rule {:key :name :type String :req-fn (fn [_] true)})) "With optional field: req-fn")
    (is (nil? (rules/check-rule {:key :name :type String :excluded []})) "With optional field: excluded")
    (is (nil? (rules/check-rule {:key :name :type String :excluded-fn (fn [_] true)})) "With optional field: excluded-fn")
    (is (nil? (rules/check-rule {:key :name :type String :name "Name Test"})) "With optional field: name")
    (is (nil? (rules/check-rule {:key :name :type String :min -2.3})) "With optional field: min")
    (is (nil? (rules/check-rule {:key :name :type String :max 19.1})) "With optional field: max")
    (is (nil? (rules/check-rule {:key :name :type String :min-length 0})) "With optional field: min-length")
    (is (nil? (rules/check-rule {:key :name :type String :max-length 9090})) "With optional field: max-length")
    (is (nil? (rules/check-rule {:key :name :type String :contains [1 2]})) "With optional field: contains")
    (is (nil? (rules/check-rule {:key :name :type String :contains #{"1" "2"}})) "With optional field: contains")
    (is (nil? (rules/check-rule {:key :name :type String :format #"\d{4}"})) "With optional field: format")
    (is (nil? (rules/check-rule {:key :name :type String :format-fn (fn [_] true)})) "With optional field: format-fn")
    (is (nil? (rules/check-rule {:key :name :type rules/Vector :of String})) "With optional field: of"))

  (testing "Invalid rules"
    (is (thrown? java.lang.AssertionError (rules/check-rule {})) "Missing required fields")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key 100 :type Integer})) "Invalid values for field: key")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type "Integer"})) "Invalid values for field: type")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :req "false"})) "Invalid values for field: req")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :req-depends some?})) "Invalid values for field: req-depends")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :req-fn #{"bla"}})) "Invalid values for field: req-fn")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :excluded false})) "Invalid values for field: excluded")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :excluded-fn #{"bla"}})) "Invalid values for field: excluded-fn")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :name 2.2})) "Invalid values for field: name")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :min "-19"})) "Invalid values for field: min")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :max true})) "Invalid values for field: max")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :min-length false})) "Invalid values for field: min-length")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :max-length "99"})) "Invalid values for field: max-length")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :contains some?})) "Invalid values for field: contains")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :format {:test 1}})) "Invalid values for field: format")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type Integer :format-fn 1})) "Invalid values for field: format-fn")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type rules/Vector :of "1"})) "Invalid values for field: of")
    (is (thrown? java.lang.AssertionError (rules/check-rule {:key :name :type String :of "1"})) "Invalid :type to use with :of")))
