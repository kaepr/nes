(defproject nes "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [io.github.nextjournal/clerk "0.13.842"
                  io.github.humbleui/humbleui "0.1.0" :git "https://github.com/HumbleUI/HumbleUI"]]
  :global-vars {*warn-on-reflection* true}
  :repl-options {:init-ns nes.core})
