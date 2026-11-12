(ns gvim.core
  (:require [clojure.string :as str])
  (:import [System.IO File]
           [Avalonia.Controls Window TextBlock TextBox Button ListBox StackPanel Grid DockPanel Border ScrollViewer Canvas RowDefinition ColumnDefinition GridLength GridUnitType Dock]
           [Avalonia.Media Color SolidColorBrush FontFamily FontWeight TextAlignment TextWrapping FormattedText Typeface FontStyle FontStretch FlowDirection]
           [Avalonia.Layout HorizontalAlignment VerticalAlignment Orientation]
           [Avalonia Thickness CornerRadius]
           [Avalonia.Input Key KeyModifiers]
           [GVimClr Ui]))

(def state-atom
  (atom {:filepath "[No Name]"
         :text ""
         :dirty? false
         :mode :normal         ;; :normal, :insert, :visual, :command, :search
         :visual-mode :char    ;; :char, :line, :block
         :visual-start 0
         :visual-start-row 1
         :visual-start-col 1
         :command-input ""
         :search-query ""
         :search-matches []
         :search-match-idx 0
         :last-normal-key nil
         :yank-buffer ""
         :undo-stack []
         :redo-stack []
         :message "-- NORMAL -- (Press : for command, i for insert, / for search)"
         :quit? false}))

;; --- NAVIGATION & TOKEN HELPERS ---

(defn esc-key? [key ctrl?]
  (or (= key "Escape")
      (and ctrl? (or (= key "OemOpenBrackets")
                     (= key "Oem4")
                     (= key "BracketLeft")
                     (= key "OpenBracket")))))

(defn char-class [c]
  (cond
    (System.Char/IsWhiteSpace c) :space
    (or (System.Char/IsLetterOrDigit c)
        (= c \_) (= c \-) (= c \?) (= c \!) (= c \*)) :word
    :else :punct))

(defn caret->row-col [text caret]
  (let [c (min (count text) (max 0 caret))
        prefix (subs text 0 c)
        lines (str/split prefix #"\r?\n" -1)
        row (count lines)
        col (inc (count (last lines)))]
    [row col]))

(defn row-col->caret [text row col]
  (let [lines (str/split text #"\r?\n" -1)
        r (min (count lines) (max 1 row))
        prefix (take (dec r) lines)
        prefix-len (reduce + (count prefix) (map count prefix))
        cur-line (nth lines (dec r) "")
        c (min (count cur-line) (max 0 (dec col)))]
    (+ prefix-len c)))

(defn get-line-bounds [text caret]
  (let [c (min (count text) (max 0 caret))
        start (let [prev (str/last-index-of (subs text 0 c) "\n")]
                (if prev (inc prev) 0))
        next-nl (str/index-of text "\n" c)
        end (if next-nl next-nl (count text))
        after (if next-nl (inc next-nl) end)]
    [start end after]))

(defn get-visual-line-bounds [text caret1 caret2]
  (let [c1 (min caret1 caret2)
        c2 (max caret1 caret2)
        [start1 _ _] (get-line-bounds text c1)
        [_ _ after2] (get-line-bounds text c2)]
    [start1 after2]))

;; --- WORD MOTIONS (w / b) ---

(defn next-word-pos [text caret]
  (let [len (count text)]
    (if (>= caret len)
      len
      (let [start-class (char-class (nth text caret))]
        (loop [i caret]
          (if (>= i len)
            len
            (let [cur-class (char-class (nth text i))]
              (if (= start-class :space)
                (if (not= cur-class :space)
                  i
                  (recur (inc i)))
                (if (= cur-class start-class)
                  (recur (inc i))
                  (loop [j i]
                    (if (>= j len)
                      len
                      (if (not= (char-class (nth text j)) :space)
                        j
                        (recur (inc j))))))))))))))

(defn prev-word-pos [text caret]
  (let [len (count text)]
    (if (<= caret 0)
      0
      (let [i (loop [idx (dec caret)]
                (if (<= idx 0)
                  0
                  (if (= (char-class (nth text idx)) :space)
                    (recur (dec idx))
                    idx)))]
        (if (<= i 0)
          0
          (let [target-class (char-class (nth text i))]
            (loop [idx i]
              (if (<= idx 0)
                0
                (if (= (char-class (nth text (dec idx))) target-class)
                  (recur (dec idx))
                  idx)))))))))

;; --- EDITING OPERATIONS (dd, yy, D, Y, J, p, P, x, ~) ---

(defn delete-current-line [text caret]
  (let [[start end after] (get-line-bounds text caret)
        line (subs text start end)
        new-text (str (subs text 0 start) (subs text after))]
    {:text new-text
     :caret (min (count new-text) start)
     :yanked (str line "\n")}))

(defn yank-current-line [text caret]
  (let [[start end _] (get-line-bounds text caret)]
    (str (subs text start end) "\n")))

(defn delete-to-end-of-line [text caret]
  (let [c (min (count text) (max 0 caret))
        next-nl (str/index-of text "\n" c)
        end (if next-nl next-nl (count text))
        yanked (subs text c end)
        new-text (str (subs text 0 c) (subs text end))]
    {:text new-text
     :caret (max 0 (min (dec (count new-text)) c))
     :yanked yanked}))

(defn yank-to-end-of-line [text caret]
  (let [c (min (count text) (max 0 caret))
        next-nl (str/index-of text "\n" c)
        end (if next-nl next-nl (count text))]
    (subs text c end)))

(defn join-next-line [text caret]
  (let [c (min (count text) (max 0 caret))
        next-nl (str/index-of text "\n" c)]
    (if-not next-nl
      {:text text :caret caret}
      (let [after-nl (inc next-nl)
            after-nl-end (let [nxt (str/index-of text "\n" after-nl)]
                           (if nxt nxt (count text)))
            next-line (subs text after-nl after-nl-end)
            trimmed-next (str/triml next-line)
            new-text (str (subs text 0 next-nl) " " trimmed-next (subs text after-nl-end))]
        {:text new-text :caret next-nl}))))

(defn paste-below [text caret yanked]
  (let [[_ _ after] (get-line-bounds text caret)
        clean-yank (if (str/ends-with? yanked "\n") yanked (str yanked "\n"))
        new-text (str (subs text 0 after) clean-yank (subs text after))]
    {:text new-text
     :caret after}))

(defn paste-above [text caret yanked]
  (let [[start _ _] (get-line-bounds text caret)
        clean-yank (if (str/ends-with? yanked "\n") yanked (str yanked "\n"))
        new-text (str (subs text 0 start) clean-yank (subs text start))]
    {:text new-text
     :caret start}))

(defn delete-char-at-caret [text caret]
  (if (or (empty? text) (>= caret (count text)))
    {:text text :caret caret}
    {:text (str (subs text 0 caret) (subs text (inc caret)))
     :caret (min (dec (count text)) caret)}))

(defn toggle-case-char [text caret]
  (if (or (empty? text) (>= caret (count text)))
    {:text text :caret caret}
    (let [ch (nth text caret)
          ch-str (str ch)
          toggled (if (= ch-str (str/upper-case ch-str))
                    (str/lower-case ch-str)
                    (str/upper-case ch-str))]
      {:text (str (subs text 0 caret) toggled (subs text (inc caret)))
       :caret (min (dec (count text)) (inc caret))})))

;; --- BLOCK-WISE (VISUAL BLOCK) HELPERS ---

(defn extract-block [text r1 c1 r2 c2]
  (let [lines (str/split text #"\r?\n" -1)
        min-r (dec (min r1 r2))
        max-r (dec (max r1 r2))
        min-c (dec (min c1 c2))
        max-c (max c1 c2)]
    (->> (subvec lines min-r (inc max-r))
         (mapv (fn [line]
                 (let [l (count line)]
                   (if (> min-c l)
                     ""
                     (subs line min-c (min l max-c))))))
         (str/join "\n"))))

(defn delete-block [text r1 c1 r2 c2]
  (let [lines (str/split text #"\r?\n" -1)
        min-r (dec (min r1 r2))
        max-r (dec (max r1 r2))
        min-c (dec (min c1 c2))
        max-c (max c1 c2)
        new-lines (mapv (fn [i line]
                          (if (and (>= i min-r) (<= i max-r))
                            (let [l (count line)]
                              (str (subs line 0 (min l min-c))
                                   (if (> l max-c) (subs line max-c) "")))
                            line))
                        (range (count lines))
                        lines)]
    (str/join "\n" new-lines)))

(defn find-matches [text query]
  (if (str/blank? query)
    []
    (loop [idx 0 acc []]
      (let [found (str/index-of (str/lower-case text) (str/lower-case query) idx)]
        (if found
          (recur (+ found (max 1 (count query))) (conj acc found))
          acc)))))

;; --- COMMAND LINE PARSER ---

(defn execute-command [cmd-str state]
  (let [trimmed (str/trim cmd-str)]
    (cond
      ;; :w or :w <filename>
      (or (= trimmed "w") (str/starts-with? trimmed "w "))
      (let [target (if (= trimmed "w")
                     (if (and (:filepath state) (not= (:filepath state) "[No Name]"))
                       (:filepath state)
                       "/tmp/gvimclr/untitled.txt")
                     (str/trim (subs trimmed 2)))]
        (try
          (spit target (:text state))
          (assoc state :filepath target
                       :dirty? false
                       :mode :normal
                       :command-input ""
                       :message (str "\"" target "\" written (" (count (:text state)) " bytes)"))
          (catch Exception e
            (assoc state :mode :normal
                         :command-input ""
                         :message (str "E212: Cannot open file for writing: " (.Message e))))))

      ;; :q! (force quit)
      (= trimmed "q!")
      (assoc state :quit? true)

      ;; :q (quit if clean)
      (= trimmed "q")
      (if (:dirty? state)
        (assoc state :mode :normal
                     :command-input ""
                     :message "E37: No write since last change (add ! to override)")
        (assoc state :quit? true))

      ;; :wq or :x (save and quit)
      (or (= trimmed "wq") (= trimmed "x"))
      (let [target (if (and (:filepath state) (not= (:filepath state) "[No Name]"))
                     (:filepath state)
                     "/tmp/gvimclr/untitled.txt")]
        (try
          (spit target (:text state))
          (assoc state :filepath target :dirty? false :quit? true)
          (catch Exception e
            (assoc state :mode :normal
                         :command-input ""
                         :message (str "E212: Cannot save: " (.Message e))))))

      ;; :e <filename> (open file)
      (str/starts-with? trimmed "e ")
      (let [target (str/trim (subs trimmed 2))]
        (try
          (if (System.IO.File/Exists target)
            (let [content (slurp target)]
              (assoc state :filepath target
                           :text content
                           :dirty? false
                           :mode :normal
                           :command-input ""
                           :message (str "\"" target "\" loaded (" (count (str/split-lines content)) " lines)")))
            (assoc state :filepath target
                         :text ""
                         :dirty? false
                         :mode :normal
                         :command-input ""
                         :message (str "\"" target "\" [New File]")))
          (catch Exception e
            (assoc state :mode :normal
                         :command-input ""
                         :message (str "E484: Cannot open file " target ": " (.Message e))))))

      ;; Jump to line number: :42
      (re-matches #"^\d+$" trimmed)
      (let [line-no (max 1 (Int32/Parse trimmed))]
        (assoc state :mode :normal
                     :command-input ""
                     :goto-line line-no
                     :message (str "Jumped to line " line-no)))

      ;; Substitution: :%s/old/new/g or :s/old/new/g
      (re-matches #"^(%?s)/([^/]+)/([^/]*)(/g)?$" trimmed)
      (let [m (re-matches #"^(%?s)/([^/]+)/([^/]*)(/g)?$" trimmed)
            old-str (nth m 2)
            new-str (nth m 3)
            global? (boolean (nth m 4))]
        (let [new-text (if global?
                         (str/replace (:text state) old-str new-str)
                         (str/replace-first (:text state) old-str new-str))]
          (assoc state :text new-text
                       :dirty? true
                       :mode :normal
                       :command-input ""
                       :message (str "Substituted '" old-str "' -> '" new-str "'"))))

      ;; :help
      (or (= trimmed "h") (= trimmed "help"))
      (assoc state :mode :normal
                   :command-input ""
                   :message "GVimCLR: h/j/k/l: Nav • v/V/^V: Visual • y/d/D/Y/C/S/s/J • :w :q :e /find")

      ;; Unknown command
      :else
      (assoc state :mode :normal
                   :command-input ""
                   :message (str "E492: Not an editor command: " trimmed)))))

;; --- MODULAR KEY HANDLERS ---

(defn handle-visual-key! [win editor cb state-atom refresh-ui! key shift? ctrl?]
  (let [st @state-atom]
    (cond
      ;; Escape or Ctrl-[ cancels visual mode back to NORMAL
      (esc-key? key ctrl?)
      (do (swap! state-atom assoc :mode :normal :message "-- NORMAL --") (refresh-ui!) true)

      ;; y (Yank visual selection)
      (= key "Y")
      (let [txt (:text st)
            caret (Ui/GetCaretIndex editor)
            v-mode (:visual-mode st)
            v-start (:visual-start st)]
        (case v-mode
          :char
          (let [s-start (min v-start caret)
                s-end (min (count txt) (inc (max v-start caret)))
                yanked (subs txt s-start s-end)]
            (Ui/SetClipboardText cb yanked)
            (swap! state-atom assoc :mode :normal :yank-buffer yanked :message (str (count yanked) " chars yanked")))

          :line
          (let [[s-start s-end] (get-visual-line-bounds txt v-start caret)
                yanked (subs txt s-start s-end)]
            (Ui/SetClipboardText cb yanked)
            (swap! state-atom assoc :mode :normal :yank-buffer yanked :message (str (count (str/split-lines yanked)) " lines yanked")))

          :block
          (let [[r1 c1] (caret->row-col txt v-start)
                [r2 c2] (caret->row-col txt caret)
                yanked (extract-block txt r1 c1 r2 c2)]
            (Ui/SetClipboardText cb yanked)
            (swap! state-atom assoc :mode :normal :yank-buffer yanked :message "Block yanked")))
        (refresh-ui!)
        true)

      ;; d or x (Delete visual selection)
      (or (= key "D") (= key "X") (= key "Delete"))
      (let [txt (:text st)
            caret (Ui/GetCaretIndex editor)
            v-mode (:visual-mode st)
            v-start (:visual-start st)]
        (case v-mode
          :char
          (let [s-start (min v-start caret)
                s-end (min (count txt) (inc (max v-start caret)))
                yanked (subs txt s-start s-end)
                new-txt (str (subs txt 0 s-start) (subs txt s-end))]
            (Ui/SetClipboardText cb yanked)
            (swap! state-atom assoc :text new-txt :mode :normal :yank-buffer yanked :dirty? true :message (str (count yanked) " chars deleted"))
            (.set_Text editor new-txt)
            (Ui/SetCaretIndex editor s-start))

          :line
          (let [[s-start s-end] (get-visual-line-bounds txt v-start caret)
                yanked (subs txt s-start s-end)
                new-txt (str (subs txt 0 s-start) (subs txt s-end))]
            (Ui/SetClipboardText cb yanked)
            (swap! state-atom assoc :text new-txt :mode :normal :yank-buffer yanked :dirty? true :message (str (count (str/split-lines yanked)) " lines deleted"))
            (.set_Text editor new-txt)
            (Ui/SetCaretIndex editor s-start))

          :block
          (let [[r1 c1] (caret->row-col txt v-start)
                [r2 c2] (caret->row-col txt caret)
                yanked (extract-block txt r1 c1 r2 c2)
                new-txt (delete-block txt r1 c1 r2 c2)]
            (Ui/SetClipboardText cb yanked)
            (swap! state-atom assoc :text new-txt :mode :normal :yank-buffer yanked :dirty? true :message "Block deleted")
            (.set_Text editor new-txt)
            (Ui/SetCaretIndex editor (row-col->caret new-txt (min r1 r2) (min c1 c2)))))
        (refresh-ui!)
        true)

      ;; c (Change visual selection)
      (= key "C")
      (let [txt (:text st)
            caret (Ui/GetCaretIndex editor)
            v-mode (:visual-mode st)
            v-start (:visual-start st)]
        (case v-mode
          :char
          (let [s-start (min v-start caret)
                s-end (min (count txt) (inc (max v-start caret)))
                yanked (subs txt s-start s-end)
                new-txt (str (subs txt 0 s-start) (subs txt s-end))]
            (Ui/SetClipboardText cb yanked)
            (swap! state-atom assoc :text new-txt :mode :insert :yank-buffer yanked :dirty? true :message "-- INSERT --")
            (.set_Text editor new-txt)
            (Ui/SetCaretIndex editor s-start))

          :line
          (let [[s-start s-end] (get-visual-line-bounds txt v-start caret)
                yanked (subs txt s-start s-end)
                new-txt (str (subs txt 0 s-start) "\n" (subs txt s-end))]
            (Ui/SetClipboardText cb yanked)
            (swap! state-atom assoc :text new-txt :mode :insert :yank-buffer yanked :dirty? true :message "-- INSERT --")
            (.set_Text editor new-txt)
            (Ui/SetCaretIndex editor s-start))

          :block
          (let [[r1 c1] (caret->row-col txt v-start)
                [r2 c2] (caret->row-col txt caret)
                yanked (extract-block txt r1 c1 r2 c2)
                new-txt (delete-block txt r1 c1 r2 c2)]
            (Ui/SetClipboardText cb yanked)
            (swap! state-atom assoc :text new-txt :mode :insert :yank-buffer yanked :dirty? true :message "-- INSERT --")
            (.set_Text editor new-txt)
            (Ui/SetCaretIndex editor (row-col->caret new-txt (min r1 r2) (min c1 c2)))))
        (refresh-ui!)
        true)

      ;; Motions in visual mode expand selection:
      (or (= key "H") (= key "Left"))
      (let [caret (Ui/GetCaretIndex editor)
            [start _ _] (get-line-bounds (:text st) caret)]
        (when (> caret start)
          (Ui/SetCaretIndex editor (dec caret))
          (refresh-ui!))
        true)

      (or (= key "L") (= key "Right"))
      (let [caret (Ui/GetCaretIndex editor)
            [_ end _] (get-line-bounds (:text st) caret)]
        (when (< caret end)
          (Ui/SetCaretIndex editor (inc caret))
          (refresh-ui!))
        true)

      (or (= key "J") (= key "Down"))
      (let [caret (Ui/GetCaretIndex editor)
            [row col] (caret->row-col (:text st) caret)
            new-caret (row-col->caret (:text st) (inc row) col)]
        (Ui/SetCaretIndex editor new-caret)
        (refresh-ui!)
        true)

      (or (= key "K") (= key "Up"))
      (let [caret (Ui/GetCaretIndex editor)
            [row col] (caret->row-col (:text st) caret)
            new-caret (row-col->caret (:text st) (max 1 (dec row)) col)]
        (Ui/SetCaretIndex editor new-caret)
        (refresh-ui!)
        true)

      (= key "W")
      (let [caret (Ui/GetCaretIndex editor)
            new-caret (next-word-pos (:text st) caret)]
        (Ui/SetCaretIndex editor new-caret)
        (refresh-ui!)
        true)

      (= key "B")
      (let [caret (Ui/GetCaretIndex editor)
            new-caret (prev-word-pos (:text st) caret)]
        (Ui/SetCaretIndex editor new-caret)
        (refresh-ui!)
        true)

      :else false)))

(defn handle-normal-key! [win editor cb state-atom refresh-ui! key shift? ctrl?]
  (let [st @state-atom
        txt (:text st)
        caret (Ui/GetCaretIndex editor)
        [row col] (caret->row-col txt caret)]
    (cond
      ;; Mode switches: i, a, I, A, o, O
      (and (= key "I") (not shift?))
      (do (swap! state-atom assoc :mode :insert) (refresh-ui!) true)

      (and (= key "I") shift?)
      (let [line-start (first (get-line-bounds txt caret))]
        (Ui/SetCaretIndex editor line-start)
        (swap! state-atom assoc :mode :insert)
        (refresh-ui!)
        true)

      (and (= key "A") (not shift?))
      (do
        (Ui/SetCaretIndex editor (min (count txt) (inc caret)))
        (swap! state-atom assoc :mode :insert)
        (refresh-ui!)
        true)

      (and (= key "A") shift?)
      (let [[_ line-end _] (get-line-bounds txt caret)]
        (Ui/SetCaretIndex editor line-end)
        (swap! state-atom assoc :mode :insert)
        (refresh-ui!)
        true)

      (and (= key "O") (not shift?))
      (let [[_ line-end _] (get-line-bounds txt caret)
            new-text (str (subs txt 0 line-end) "\n" (subs txt line-end))]
        (swap! state-atom assoc :text new-text :dirty? true :mode :insert)
        (.set_Text editor new-text)
        (Ui/SetCaretIndex editor (inc line-end))
        (refresh-ui!)
        true)

      (and (= key "O") shift?)
      (let [[line-start _ _] (get-line-bounds txt caret)
            new-text (str (subs txt 0 line-start) "\n" (subs txt line-start))]
        (swap! state-atom assoc :text new-text :dirty? true :mode :insert)
        (.set_Text editor new-text)
        (Ui/SetCaretIndex editor line-start)
        (refresh-ui!)
        true)

      ;; Visual Modes: v (char), V (line), Ctrl-V (block)
      (and (= key "V") (not shift?) (not ctrl?))
      (do
        (swap! state-atom assoc :mode :visual :visual-mode :char :visual-start caret)
        (refresh-ui!)
        true)

      (and (= key "V") shift? (not ctrl?))
      (do
        (swap! state-atom assoc :mode :visual :visual-mode :line :visual-start caret)
        (refresh-ui!)
        true)

      (and (= key "V") ctrl?)
      (do
        (swap! state-atom assoc :mode :visual :visual-mode :block :visual-start caret
                                :visual-start-row row :visual-start-col col)
        (refresh-ui!)
        true)

      ;; Command line mode: :
      (or (= key "OemSemicolon") (= key "Colon") (and (= key "Oem1") shift?))
      (do
        (swap! state-atom assoc :mode :command)
        (refresh-ui!)
        true)

      ;; Search mode: /
      (or (= key "Oem2") (= key "Divide") (= key "Slash"))
      (do
        (swap! state-atom assoc :mode :search)
        (refresh-ui!)
        true)

      ;; Navigation: h, j, k, l, arrows
      (or (= key "H") (= key "Left"))
      (let [[start _ _] (get-line-bounds txt caret)]
        (when (> caret start)
          (Ui/SetCaretIndex editor (dec caret))
          (refresh-ui!))
        true)

      (or (= key "L") (= key "Right"))
      (let [[_ end _] (get-line-bounds txt caret)]
        (when (< caret end)
          (Ui/SetCaretIndex editor (inc caret))
          (refresh-ui!))
        true)

      (or (= key "J") (= key "Down"))
      (if (and (= key "J") shift?)
        (let [{:keys [text caret]} (join-next-line txt caret)]
          (swap! state-atom assoc :text text :dirty? true :message "Joined lines")
          (.set_Text editor text)
          (Ui/SetCaretIndex editor caret)
          (refresh-ui!)
          true)
        (let [target-row (inc row)
              new-caret (row-col->caret txt target-row col)]
          (Ui/SetCaretIndex editor new-caret)
          (refresh-ui!)
          true))

      (or (= key "K") (= key "Up"))
      (let [target-row (max 1 (dec row))
            new-caret (row-col->caret txt target-row col)]
        (Ui/SetCaretIndex editor new-caret)
        (refresh-ui!)
        true)

      ;; Word jump: w, b
      (= key "W")
      (let [new-caret (next-word-pos txt caret)]
        (Ui/SetCaretIndex editor new-caret)
        (refresh-ui!))
      true

      (= key "B")
      (let [new-caret (prev-word-pos txt caret)]
        (Ui/SetCaretIndex editor new-caret)
        (refresh-ui!))
      true

      ;; Line start / end: 0, $
      (= key "D0")
      (let [[start _ _] (get-line-bounds txt caret)]
        (Ui/SetCaretIndex editor start)
        (refresh-ui!)
        true)

      (and (= key "D4") shift?)
      (let [[_ end _] (get-line-bounds txt caret)]
        (Ui/SetCaretIndex editor end)
        (refresh-ui!)
        true)

      ;; Top / Bottom: gg / G
      (and (= key "G") (not shift?))
      (if (= (:last-normal-key st) "G")
        (do
          (swap! state-atom assoc :last-normal-key nil)
          (Ui/SetCaretIndex editor 0)
          (refresh-ui!)
          true)
        (do
          (swap! state-atom assoc :last-normal-key "G")
          true))

      (and (= key "G") shift?)
      (let [lines (str/split txt #"\r?\n" -1)
            last-row (count lines)
            new-caret (row-col->caret txt last-row 1)]
        (swap! state-atom assoc :last-normal-key nil)
        (Ui/SetCaretIndex editor new-caret)
        (refresh-ui!)
        true)

      ;; Delete char: x
      (= key "X")
      (let [{:keys [text caret]} (delete-char-at-caret txt caret)]
        (swap! state-atom assoc :text text :dirty? true)
        (.set_Text editor text)
        (Ui/SetCaretIndex editor caret)
        (refresh-ui!)
        true)

      ;; D: Delete to end of line
      (and (= key "D") shift?)
      (let [{:keys [text caret yanked]} (delete-to-end-of-line txt caret)]
        (Ui/SetClipboardText cb yanked)
        (swap! state-atom assoc :text text :yank-buffer yanked :dirty? true :message (str "Deleted: " (pr-str yanked)))
        (.set_Text editor text)
        (Ui/SetCaretIndex editor caret)
        (refresh-ui!)
        true)

      ;; Y: Yank to end of line
      (and (= key "Y") shift?)
      (let [yanked (yank-to-end-of-line txt caret)]
        (Ui/SetClipboardText cb yanked)
        (swap! state-atom assoc :yank-buffer yanked :message (str "Yanked: " (pr-str yanked)))
        (refresh-ui!)
        true)

      ;; C: Change to end of line
      (and (= key "C") shift?)
      (let [{:keys [text caret yanked]} (delete-to-end-of-line txt caret)]
        (Ui/SetClipboardText cb yanked)
        (swap! state-atom assoc :text text :yank-buffer yanked :dirty? true :mode :insert :message "-- INSERT --")
        (.set_Text editor text)
        (Ui/SetCaretIndex editor caret)
        (refresh-ui!)
        true)

      ;; S: Change whole line
      (and (= key "S") shift?)
      (let [{:keys [text caret yanked]} (delete-current-line txt caret)]
        (Ui/SetClipboardText cb yanked)
        (swap! state-atom assoc :text text :yank-buffer yanked :dirty? true :mode :insert :message "-- INSERT --")
        (.set_Text editor text)
        (Ui/SetCaretIndex editor caret)
        (refresh-ui!)
        true)

      ;; s: Substitute character
      (and (= key "S") (not shift?))
      (let [{:keys [text caret]} (delete-char-at-caret txt caret)]
        (swap! state-atom assoc :text text :dirty? true :mode :insert :message "-- INSERT --")
        (.set_Text editor text)
        (Ui/SetCaretIndex editor caret)
        (refresh-ui!)
        true)

      ;; dd (delete line)
      (and (= key "D") (not shift?))
      (if (= (:last-normal-key st) "D")
        (let [{:keys [text caret yanked]} (delete-current-line txt caret)]
          (Ui/SetClipboardText cb yanked)
          (swap! state-atom assoc :text text :yank-buffer yanked :dirty? true :last-normal-key nil :message "1 line deleted")
          (.set_Text editor text)
          (Ui/SetCaretIndex editor caret)
          (refresh-ui!)
          true)
        (do
          (swap! state-atom assoc :last-normal-key "D")
          true))

      ;; yy (yank line)
      (and (= key "Y") (not shift?))
      (if (= (:last-normal-key st) "Y")
        (let [yanked (yank-current-line txt caret)]
          (Ui/SetClipboardText cb yanked)
          (swap! state-atom assoc :yank-buffer yanked :last-normal-key nil :message "1 line yanked")
          (refresh-ui!)
          true)
        (do
          (swap! state-atom assoc :last-normal-key "Y")
          true))

      ;; Paste: p, P
      (and (= key "P") (not shift?))
      (let [yanked (or (Ui/GetClipboardText cb) (:yank-buffer st))]
        (when-not (str/blank? yanked)
          (let [{:keys [text caret]} (paste-below txt caret yanked)]
            (swap! state-atom assoc :text text :dirty? true :message "Pasted below")
            (.set_Text editor text)
            (Ui/SetCaretIndex editor caret)
            (refresh-ui!)))
        true)

      (and (= key "P") shift?)
      (let [yanked (or (Ui/GetClipboardText cb) (:yank-buffer st))]
        (when-not (str/blank? yanked)
          (let [{:keys [text caret]} (paste-above txt caret yanked)]
            (swap! state-atom assoc :text text :dirty? true :message "Pasted above")
            (.set_Text editor text)
            (Ui/SetCaretIndex editor caret)
            (refresh-ui!)))
        true)

      ;; Undo: u
      (and (= key "U") (not ctrl?))
      (let [prev (first (:undo-stack st))]
        (when prev
          (swap! state-atom assoc
                 :text prev
                 :undo-stack (rest (:undo-stack st))
                 :redo-stack (conj (:redo-stack st) txt)
                 :message "1 change undone")
          (.set_Text editor prev)
          (refresh-ui!))
        true)

      ;; Redo: Ctrl+R
      (and (= key "R") ctrl?)
      (let [next-txt (first (:redo-stack st))]
        (when next-txt
          (swap! state-atom assoc
                 :text next-txt
                 :undo-stack (conj (:undo-stack st) txt)
                 :redo-stack (rest (:redo-stack st))
                 :message "1 change redone")
          (.set_Text editor next-txt)
          (refresh-ui!))
        true)

      ;; Search next / prev: n, N
      (and (= key "N") (not shift?))
      (let [matches (:search-matches st)]
        (when (seq matches)
          (let [idx (mod (inc (:search-match-idx st)) (count matches))
                target (nth matches idx)]
            (swap! state-atom assoc :search-match-idx idx :message (str "/" (:search-query st) " [" (inc idx) " of " (count matches) "]"))
            (Ui/SetCaretIndex editor target)
            (refresh-ui!)))
        true)

      (and (= key "N") shift?)
      (let [matches (:search-matches st)]
        (when (seq matches)
          (let [idx (mod (dec (:search-match-idx st)) (count matches))
                target (nth matches idx)]
          (swap! state-atom assoc :search-match-idx idx :message (str "/" (:search-query st) " [" (inc idx) " of " (count matches) "]"))
          (Ui/SetCaretIndex editor target)
          (refresh-ui!)))
        true)

      ;; Toggle case: ~
      (and (= key "OemTilde") shift?)
      (let [{:keys [text caret]} (toggle-case-char txt caret)]
        (swap! state-atom assoc :text text :dirty? true)
        (.set_Text editor text)
        (Ui/SetCaretIndex editor caret)
        (refresh-ui!)
        true)

      :else false)))

(defn handle-cmd-box-key! [editor cmd-box state-atom refresh-ui! key shift? ctrl?]
  (cond
    (= key "Enter")
    (let [input (.get_Text cmd-box)
          st @state-atom]
      (if (= (:mode st) :command)
        (let [res (execute-command input st)]
          (reset! state-atom res)
          (.set_Text cmd-box "")
          (Ui/FocusElement editor)
          (when-let [g (:goto-line res)]
            (let [new-caret (row-col->caret (:text res) g 1)]
              (Ui/SetCaretIndex editor new-caret)))
          (refresh-ui!)
          true)
        (let [matches (find-matches (:text st) input)]
          (if (empty? matches)
            (do
              (swap! state-atom assoc :mode :normal :message (str "E486: Pattern not found: " input))
              (.set_Text cmd-box "")
              (Ui/FocusElement editor)
              (refresh-ui!)
              true)
            (let [first-match (first matches)]
              (swap! state-atom assoc :mode :normal
                                      :search-query input
                                      :search-matches matches
                                      :search-match-idx 0
                                      :message (str "/" input " [1 of " (count matches) "]"))
              (.set_Text cmd-box "")
              (Ui/FocusElement editor)
              (Ui/SetCaretIndex editor first-match)
              (refresh-ui!)
              true)))))

    (esc-key? key ctrl?)
    (do
      (swap! state-atom assoc :mode :normal :message "-- NORMAL --")
      (.set_Text cmd-box "")
      (Ui/FocusElement editor)
      (refresh-ui!)
      true)

    :else false))

;; --- MODULAR GUI BUILDERS ---

(defn make-brushes []
  {:bg (SolidColorBrush/Parse "#1e1e2e")
   :gutter (SolidColorBrush/Parse "#181825")
   :toolbar (SolidColorBrush/Parse "#282a36")
   :text-main (SolidColorBrush/Parse "#cdd6f4")
   :text-dim (SolidColorBrush/Parse "#6c7086")
   :accent (SolidColorBrush/Parse "#89b4fa")
   :danger (SolidColorBrush/Parse "#f38ba8")
   :success (SolidColorBrush/Parse "#a6e3a1")
   :warning (SolidColorBrush/Parse "#fab387")
   :command (SolidColorBrush/Parse "#cba6f7")
   :dark (SolidColorBrush/Parse "#11111b")
   :block (SolidColorBrush/Parse "#44fab387")})

(defn measure-font [font-mono brush-text-main]
  (let [tf (Typeface. font-mono FontStyle/Normal FontWeight/Normal FontStretch/Normal)
        ft (FormattedText. "M"
                           System.Globalization.CultureInfo/InvariantCulture
                           FlowDirection/LeftToRight
                           tf
                           14.0
                           brush-text-main)]
    [(.get_Width ft) (.get_Height ft)]))

(defn make-toolbar [brushes state-atom editor cb refresh-ui!]
  (let [tb (StackPanel.)
        _ (.set_Orientation tb Orientation/Horizontal)
        _ (.set_Background tb (:toolbar brushes))
        _ (DockPanel/SetDock tb Dock/Top)
        make-btn (fn [label f]
                   (let [b (Button.)]
                     (.set_Content b label)
                     (.set_Background b (:toolbar brushes))
                     (.set_Foreground b (:text-main brushes))
                     (.set_Margin b (Thickness. 4.0 4.0 2.0 4.0))
                     (Ui/OnClick b f)
                     b))]
    (doto (.get_Children tb)
      (.Add (make-btn "📄 New"
                      (fn []
                        (swap! state-atom assoc :text "" :filepath "[No Name]" :dirty? false :message "[New File]")
                        (.set_Text editor "")
                        (refresh-ui!))))
      (.Add (make-btn "📂 Open [:e]"
                      (fn []
                        (swap! state-atom assoc :mode :command :command-input "e ")
                        (refresh-ui!))))
      (.Add (make-btn "💾 Save [:w]"
                      (fn []
                        (let [res (execute-command "w" @state-atom)]
                          (reset! state-atom res)
                          (refresh-ui!)))))
      (.Add (make-btn "↩️ Undo [u]"
                      (fn []
                        (let [st @state-atom
                              prev (first (:undo-stack st))]
                          (when prev
                            (swap! state-atom assoc
                                   :text prev
                                   :undo-stack (rest (:undo-stack st))
                                   :redo-stack (conj (:redo-stack st) (:text st))
                                   :message "1 change undone")
                            (.set_Text editor prev)
                            (refresh-ui!))))))
      (.Add (make-btn "↪️ Redo [^R]"
                      (fn []
                        (let [st @state-atom
                              next-txt (first (:redo-stack st))]
                          (when next-txt
                            (swap! state-atom assoc
                                   :text next-txt
                                   :undo-stack (conj (:undo-stack st) (:text st))
                                   :redo-stack (rest (:redo-stack st))
                                   :message "1 change redone")
                            (.set_Text editor next-txt)
                            (refresh-ui!))))))
      (.Add (make-btn "✂️ Cut [dd]"
                      (fn []
                        (let [caret (Ui/GetCaretIndex editor)
                              {:keys [text caret yanked]} (delete-current-line (:text @state-atom) caret)]
                          (Ui/SetClipboardText cb yanked)
                          (swap! state-atom assoc :text text :yank-buffer yanked :dirty? true :message "1 line cut")
                          (.set_Text editor text)
                          (Ui/SetCaretIndex editor caret)
                          (refresh-ui!)))))
      (.Add (make-btn "📋 Copy [yy]"
                      (fn []
                        (let [caret (Ui/GetCaretIndex editor)
                              yanked (yank-current-line (:text @state-atom) caret)]
                          (Ui/SetClipboardText cb yanked)
                          (swap! state-atom assoc :yank-buffer yanked :message "1 line yanked")
                          (refresh-ui!)))))
      (.Add (make-btn "📋 Paste [p]"
                      (fn []
                        (let [caret (Ui/GetCaretIndex editor)
                              clip-text (or (Ui/GetClipboardText cb) (:yank-buffer @state-atom))]
                          (when-not (str/blank? clip-text)
                            (let [{:keys [text caret]} (paste-below (:text @state-atom) caret clip-text)]
                              (swap! state-atom assoc :text text :dirty? true :message "Pasted text")
                              (.set_Text editor text)
                              (Ui/SetCaretIndex editor caret)
                              (refresh-ui!))))))))
    tb))

(defn make-statusline [brushes]
  (let [sp (DockPanel.)
        _ (.set_Background sp (:gutter brushes))
        _ (.set_Height sp 26.0)

        mode-badge (Border.)
        _ (.set_Background mode-badge (:success brushes))
        _ (.set_Padding mode-badge (Thickness. 8.0 2.0 8.0 2.0))
        _ (DockPanel/SetDock mode-badge Dock/Left)

        mode-text (TextBlock.)
        _ (.set_Text mode-text " NORMAL ")
        _ (.set_FontWeight mode-text FontWeight/Bold)
        _ (.set_FontSize mode-text 11.0)
        _ (.set_Foreground mode-text (:dark brushes))
        _ (.set_Child mode-badge mode-text)

        file-info (TextBlock.)
        _ (.set_Text file-info "[No Name]")
        _ (.set_Foreground file-info (:text-main brushes))
        _ (.set_VerticalAlignment file-info VerticalAlignment/Center)
        _ (.set_Margin file-info (Thickness. 8.0 0.0 0.0 0.0))
        _ (DockPanel/SetDock file-info Dock/Left)

        pos-info (TextBlock.)
        _ (.set_Text pos-info "ln 1, col 1 • 7L")
        _ (.set_Foreground pos-info (:text-dim brushes))
        _ (.set_VerticalAlignment pos-info VerticalAlignment/Center)
        _ (.set_Margin pos-info (Thickness. 0.0 0.0 8.0 0.0))
        _ (.set_HorizontalAlignment pos-info HorizontalAlignment/Right)
        _ (DockPanel/SetDock pos-info Dock/Right)]

    (doto (.get_Children sp)
      (.Add mode-badge)
      (.Add pos-info)
      (.Add file-info))

    {:panel sp :mode-badge mode-badge :mode-text mode-text :file-info file-info :pos-info pos-info}))

(defn make-cmdline [brushes font-mono editor state-atom refresh-ui!]
  (let [dp (DockPanel.)
        _ (.set_Background dp (:dark brushes))
        _ (.set_Height dp 26.0)
        _ (.set_Margin dp (Thickness. 4.0 1.0 4.0 1.0))

        cmd-prompt (TextBlock.)
        _ (.set_Text cmd-prompt ":")
        _ (.set_FontFamily cmd-prompt font-mono)
        _ (.set_FontWeight cmd-prompt FontWeight/Bold)
        _ (.set_Foreground cmd-prompt (:accent brushes))
        _ (.set_VerticalAlignment cmd-prompt VerticalAlignment/Center)
        _ (.set_Margin cmd-prompt (Thickness. 0.0 0.0 4.0 0.0))
        _ (DockPanel/SetDock cmd-prompt Dock/Left)

        cmd-box (TextBox.)
        _ (.set_FontFamily cmd-box font-mono)
        _ (.set_Background cmd-box (:dark brushes))
        _ (.set_Foreground cmd-box (:text-main brushes))
        _ (.set_BorderThickness cmd-box (Thickness. 0.0))
        _ (.set_VerticalAlignment cmd-box VerticalAlignment/Center)
        _ (DockPanel/SetDock cmd-box Dock/Left)
        _ (Ui/OnKeyDown cmd-box (fn [k s c a] (handle-cmd-box-key! editor cmd-box state-atom refresh-ui! k s c)))

        msg-block (TextBlock.)
        _ (.set_Text msg-block "-- NORMAL --")
        _ (.set_FontFamily msg-block font-mono)
        _ (.set_Foreground msg-block (:text-dim brushes))
        _ (.set_VerticalAlignment msg-block VerticalAlignment/Center)
        _ (DockPanel/SetDock msg-block Dock/Left)]

    (doto (.get_Children dp)
      (.Add cmd-prompt)
      (.Add cmd-box)
      (.Add msg-block))

    {:panel dp :prompt cmd-prompt :box cmd-box :msg msg-block}))

(defn make-editor-area [brushes font-mono init-text state-atom refresh-ui!]
  (let [grid (Grid.)
        _ (doto (.get_ColumnDefinitions grid)
            (.Add (ColumnDefinition. (GridLength. 48.0 GridUnitType/Pixel)))
            (.Add (ColumnDefinition. (GridLength. 1.0 GridUnitType/Star))))

        gutter-border (Border.)
        _ (.set_Background gutter-border (:gutter brushes))
        _ (Grid/SetColumn gutter-border 0)

        gutter-text (TextBlock.)
        _ (.set_FontFamily gutter-text font-mono)
        _ (.set_FontSize gutter-text 14.0)
        _ (.set_Foreground gutter-text (:text-dim brushes))
        _ (.set_TextAlignment gutter-text TextAlignment/Right)
        _ (.set_Margin gutter-text (Thickness. 2.0 4.0 6.0 4.0))
        _ (.set_Child gutter-border gutter-text)

        editor (TextBox.)
        _ (.set_FontFamily editor font-mono)
        _ (.set_FontSize editor 14.0)
        _ (.set_Background editor (:bg brushes))
        _ (.set_Foreground editor (:text-main brushes))
        _ (.set_BorderThickness editor (Thickness. 0.0))
        _ (.set_AcceptsReturn editor true)
        _ (.set_AcceptsTab editor true)
        _ (.set_TextWrapping editor TextWrapping/NoWrap)
        _ (.set_Padding editor (Thickness. 4.0 4.0 4.0 4.0))
        _ (.set_Text editor init-text)
        _ (Grid/SetColumn editor 1)
        _ (Ui/OnTextChanged editor
            (fn [new-text]
              (let [old-text (:text @state-atom)]
                (when (not= old-text new-text)
                  (swap! state-atom
                         (fn [s]
                           (assoc s :text new-text
                                    :dirty? true
                                    :undo-stack (take 50 (conj (:undo-stack s) old-text))
                                    :redo-stack [])))
                  (refresh-ui!)))))

        overlay-canvas (Canvas.)
        _ (.set_IsHitTestVisible overlay-canvas false)
        _ (Grid/SetColumn overlay-canvas 1)

        block-rect (Border.)
        _ (.set_Background block-rect (:block brushes))
        _ (.set_BorderBrush block-rect (:warning brushes))
        _ (.set_BorderThickness block-rect (Thickness. 1.0))
        _ (.set_CornerRadius block-rect (CornerRadius. 2.0))
        _ (.set_IsVisible block-rect false)
        _ (.Add (.get_Children overlay-canvas) block-rect)]

    (doto (.get_Children grid)
      (.Add gutter-border)
      (.Add editor)
      (.Add overlay-canvas))

    {:grid grid :gutter gutter-text :editor editor :block-rect block-rect}))

;; --- UI REFRESH DISPATCHER ---

(defn update-ui! [editor gutter-text block-rect mode-badge mode-text file-info pos-info msg-block cmd-prompt cmd-box win brushes st char-width line-height]
  (let [txt (:text st)
        lines (str/split txt #"\r?\n" -1)
        line-count (max 1 (count lines))
        gutter-str (str/join "\n" (range 1 (inc line-count)))
        caret (Ui/GetCaretIndex editor)
        [row col] (caret->row-col txt caret)
        len (count txt)
        c (max 0 (min (dec len) caret))
        ch (when (< c len) (nth txt c))
        on-newline? (or (nil? ch) (= ch \newline) (= ch \return))
        brush-success (:success brushes)
        brush-accent (:accent brushes)
        brush-warning (:warning brushes)
        brush-command (:command brushes)
        brush-dark (:dark brushes)]

    (.set_Text gutter-text gutter-str)
    (case (:mode st)
      :normal
      (do
        (.set_Background mode-badge brush-success)
        (.set_Text mode-text " NORMAL ")
        (.set_IsVisible cmd-prompt false)
        (.set_IsVisible cmd-box false)
        (.set_IsVisible msg-block true)
        (.set_Text msg-block (or (:message st) "-- NORMAL --"))
        (.set_IsVisible block-rect false)
        (if on-newline?
          (do
            (.set_CaretBrush editor brush-success)
            (Ui/SetCaretIndex editor (min len caret))
            (Ui/SelectRange editor (min len caret) (min len caret)))
          (do
            (.set_SelectionBrush editor brush-success)
            (.set_SelectionForegroundBrush editor brush-dark)
            (.set_CaretBrush editor brush-success)
            (Ui/SetCaretIndex editor c)
            (Ui/SelectRange editor c (min len (inc c))))))

      :insert
      (do
        (.set_Background mode-badge brush-accent)
        (.set_Text mode-text " INSERT ")
        (.set_IsVisible cmd-prompt false)
        (.set_IsVisible cmd-box false)
        (.set_IsVisible msg-block true)
        (.set_Text msg-block "-- INSERT --")
        (.set_IsVisible block-rect false)
        (.set_CaretBrush editor brush-accent)
        (Ui/SelectRange editor caret caret))

      :visual
      (let [v-mode (:visual-mode st)
            v-start (or (:visual-start st) caret)]
        (.set_Background mode-badge brush-warning)
        (.set_IsVisible cmd-prompt false)
        (.set_IsVisible cmd-box false)
        (.set_IsVisible msg-block true)
        (.set_SelectionBrush editor brush-warning)
        (.set_SelectionForegroundBrush editor brush-dark)
        (case v-mode
          :char
          (let [s-start (min v-start caret)
                s-end (min len (inc (max v-start caret)))]
            (.set_Text mode-text " VISUAL ")
            (.set_Text msg-block (str "-- VISUAL (" (- s-end s-start) "c) --"))
            (.set_IsVisible block-rect false)
            (.set_CaretBrush editor brush-warning)
            (Ui/SelectRange editor s-start s-end))

          :line
          (let [[s-start s-end] (get-visual-line-bounds txt v-start caret)
                [r1 _] (caret->row-col txt v-start)
                [r2 _] (caret->row-col txt caret)
                lines-sel (inc (Math/Abs (- r1 r2)))]
            (.set_Text mode-text " V-LINE ")
            (.set_Text msg-block (str "-- VISUAL LINE (" lines-sel "L) --"))
            (.set_IsVisible block-rect false)
            (.set_CaretBrush editor brush-warning)
            (Ui/SelectRange editor s-start s-end))

          :block
          (let [[r1 c1] (caret->row-col txt v-start)
                [r2 c2] [row col]
                min-r (min r1 r2) max-r (max r1 r2)
                min-c (min c1 c2) max-c (max c1 c2)
                h-lines (inc (- max-r min-r))
                w-cols (inc (- max-c min-c))
                rx (+ 4.0 (* char-width (dec min-c)))
                ry (+ 4.0 (* line-height (dec min-r)))
                rw (* char-width w-cols)
                rh (* line-height h-lines)]
            (.set_Text mode-text " V-BLOCK ")
            (.set_Text msg-block (str "-- VISUAL BLOCK (" h-lines "x" w-cols ") --"))
            (Canvas/SetLeft block-rect rx)
            (Canvas/SetTop block-rect ry)
            (.set_Width block-rect rw)
            (.set_Height block-rect rh)
            (.set_IsVisible block-rect true)
            (.set_CaretBrush editor brush-warning)
            (Ui/SelectRange editor caret caret))))

      :command
      (do
        (.set_Background mode-badge brush-command)
        (.set_Text mode-text " COMMAND ")
        (.set_IsVisible cmd-prompt true)
        (.set_Text cmd-prompt ":")
        (.set_IsVisible cmd-box true)
        (.set_IsVisible msg-block false)
        (.set_IsVisible block-rect false)
        (.set_CaretBrush cmd-box brush-command)
        (Ui/FocusElement cmd-box))

      :search
      (do
        (.set_Background mode-badge brush-warning)
        (.set_Text mode-text " SEARCH ")
        (.set_IsVisible cmd-prompt true)
        (.set_Text cmd-prompt "/")
        (.set_IsVisible cmd-box true)
        (.set_IsVisible msg-block false)
        (.set_IsVisible block-rect false)
        (.set_CaretBrush cmd-box brush-warning)
        (Ui/FocusElement cmd-box)))

    (.set_Text file-info (str (:filepath st) (when (:dirty? st) " [+]")))
    (.set_Title win (str "gvim - " (:filepath st) (when (:dirty? st) " [+]") " - GVimCLR"))
    (.set_Text pos-info (str "ln " row ", col " col " • " line-count "L"))
    (when (:quit? st)
      (.Close win))))

;; --- MAIN WINDOW BUILDER ---

(defn build-window []
  (let [win (Window.)
        cb (.get_Clipboard win)
        init-text (str ";; Welcome to GVimCLR - ClojureCLR + Avalonia\n"
                       ";; A native Vim clone running on Linux (.NET 10)\n"
                       "\n"
                       "(defn greet [name]\n"
                       "  (str \"Hello, \" name \"!\"))\n"
                       "\n"
                       "(println (greet \"ClojureCLR\"))\n")
        _ (swap! state-atom assoc :text init-text :filepath "[No Name]")

        brushes (make-brushes)
        font-mono (FontFamily. "Consolas, Menlo, DejaVu Sans Mono, monospace")
        [char-width line-height] (measure-font font-mono (:text-main brushes))

        _ (.set_Title win "gvim [GVimCLR] - ClojureCLR + Avalonia")
        _ (.set_Width win 500.0)
        _ (.set_Height win 370.0)
        _ (.set_MinWidth win 320.0)
        _ (.set_MinHeight win 240.0)
        _ (.set_Background win (:bg brushes))

        root (DockPanel.)
        _ (.set_Margin root (Thickness. 0.0))

        bottom-dock (StackPanel.)
        _ (DockPanel/SetDock bottom-dock Dock/Bottom)

        status-res (make-statusline brushes)
        cmdline-res (make-cmdline brushes font-mono nil state-atom nil)
        editor-res (make-editor-area brushes font-mono init-text state-atom nil)

        editor (:editor editor-res)
        gutter-text (:gutter editor-res)
        block-rect (:block-rect editor-res)

        cmd-box (:box cmdline-res)
        cmd-prompt (:prompt cmdline-res)
        msg-block (:msg cmdline-res)

        mode-badge (:mode-badge status-res)
        mode-text (:mode-text status-res)
        file-info (:file-info status-res)
        pos-info (:pos-info status-res)

        refresh-ui! (fn []
                      (update-ui! editor gutter-text block-rect mode-badge mode-text file-info pos-info msg-block cmd-prompt cmd-box win brushes @state-atom char-width line-height))

        toolbar (make-toolbar brushes state-atom editor cb refresh-ui!)]

    ;; Connect cmd-box KeyDown with proper refresh-ui!
    (Ui/OnKeyDown cmd-box (fn [k s c a] (handle-cmd-box-key! editor cmd-box state-atom refresh-ui! k s c)))

    (doto (.get_Children bottom-dock)
      (.Add (:panel status-res))
      (.Add (:panel cmdline-res)))

    (doto (.get_Children root)
      (.Add toolbar)
      (.Add bottom-dock)
      (.Add (:grid editor-res)))

    (.set_Content win root)
    (refresh-ui!)

    (Ui/OnKeyDown win
      (fn [key shift? ctrl? alt?]
        (let [st @state-atom
              mode (:mode st)]
          (cond
            (= mode :insert)
            (if (esc-key? key ctrl?)
              (do
                (swap! state-atom assoc :mode :normal :message "-- NORMAL --")
                (let [caret (Ui/GetCaretIndex editor)]
                  (Ui/SetCaretIndex editor (max 0 (dec caret))))
                (refresh-ui!)
                true)
              false)

            (= mode :visual)
            (handle-visual-key! win editor cb state-atom refresh-ui! key shift? ctrl?)

            (or (= mode :command) (= mode :search))
            (if (esc-key? key ctrl?)
              (do
                (swap! state-atom assoc :mode :normal :message "-- NORMAL --")
                (.set_Text cmd-box "")
                (Ui/FocusElement editor)
                (refresh-ui!)
                true)
              false)

            (= mode :normal)
            (handle-normal-key! win editor cb state-atom refresh-ui! key shift? ctrl?)

            :else false))))

    win))
