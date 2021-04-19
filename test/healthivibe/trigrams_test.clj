(ns healthivibe.trigrams-test
  (:require [clojure.test :refer :all]
            [healthivibe.trigrams :refer :all]))

(def simple-example "the cat sat on the other cat on the mat on the-floor")

(deftest test-generate-trigram-map
  (testing "Simple test"
    (is (= {["the" "other"] ["cat"]
            ["mat" "on"] ["the-floor"]
            ["cat" "on"] ["the"]
            ["cat" "sat"] ["on"]
            ["the" "mat"] ["on"]
            ["on" "the"] ["other" "mat"]
            ["the" "cat"] ["sat"]
            ["sat" "on"] ["the"]
            ["other" "cat"] ["on"]}

           (generate-trigram-map simple-example)))))
