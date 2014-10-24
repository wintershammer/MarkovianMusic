(ns markovian-music.midiparser
  (:import (java.io File)
           (javax.sound.midi MidiSystem Sequence MidiMessage MidiEvent ShortMessage Track)))

;;;; BASED ON: http://stackoverflow.com/questions/3850688/reading-midi-files-in-java/3850885#3850885 
;;;; and http://vishnumenon.com/2013/06/25/musical-chains-music-generation-with-clojure/ 		   
		   
		   
(defn add-note [msg notes] 
  (let [key (.getData1 msg) 
        velocity (.getData2 msg)] 
    (if (> velocity 0) 
      (assoc notes key 
        (+ (.getData2 msg) (get notes key 0))) (dissoc notes key))))

(defn parse-midi-file
  ([file-name] (parse-midi-file file-name 0))
  ([file-name track] 
   (let [note-on 0x90
         note-off 0x80
         sequence (MidiSystem/getSequence (File. file-name))
         track  (-> sequence .getTracks (aget track))]
     (loop [current-notes {}
            parsed []
            last-time 0
            event-index 0]
       (let [event (.get track event-index)
             message (.getMessage event)]
         (cond
           (= (inc event-index) (.size track)) parsed
           (not (instance? ShortMessage message)) 
             (recur current-notes parsed last-time (inc event-index))
           (= (.getCommand message) note-on) 
             (if (= (.getTick event) last-time)
               (recur 
                 (add-note message current-notes)
                 parsed
                 last-time
                 (inc event-index))
               (recur
                 (add-note message current-notes)
                 (conj parsed 
                       {:sound current-notes 
                        :duration (- (.getTick event) last-time)})
                 (.getTick event)
                 (inc event-index)))
           (= (.getCommand message) note-off) 
             (if (= (.getTick event) last-time)
               (recur
                 (dissoc current-notes (.getData1 message))
                 parsed
                 last-time
                 (inc event-index))
               (recur
                 (dissoc current-notes (.getData1 message))
                 (conj parsed 
                       {:sound current-notes 
                        :duration (- (.getTick event) last-time)})
                 (.getTick event)
                 (inc event-index)))
           :else (recur current-notes parsed last-time (inc event-index))))))))

