(ns url-matcher.core-test
  (:require [clojure.test :refer :all]
            [url-matcher.core :as core]))


(def twitter (core/url-pattern "host(twitter.com); path(?user/status/?id);"))

(def dribbble (core/url-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))

(def some-site (core/url-pattern "host(some-site.com);   path(?foo/bar/?baz/?qux); queryparam(offset=?o)  ; queryparam(limit=?l); "))


(deftest twitter-recognize
  (is (= (core/recognize twitter "http://twitter.com/bradfitz/status/562360748727611392")
        [[:user "bradfitz"] [:id "562360748727611392"]])))


(deftest dribble-recognize-1
  (is (= (core/recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
        [[:id "1905065-Travel-Icons-pack"] [:offset "1"]])))


(deftest dribble-recognize-2
  (is (= (core/recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
        nil)))


(deftest dribble-recognize-3
  (is (= (core/recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")
        nil)))


(deftest some-site-recognize
  (is (= (core/recognize some-site "https://some-site.com/111/bar/222/333?limit=10&offset=72&gg=22")
        [[:foo "111"] [:baz "222"] [:qux "333"] [:o "72"] [:l "10"]])))
