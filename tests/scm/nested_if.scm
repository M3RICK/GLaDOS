(define x 10)
(if (< x 0)
    "negative"
    (if (eq? x 0)
        "zero"
        "positive"))