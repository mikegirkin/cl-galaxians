
.PHONY: run test

run:
	sbcl --eval "(ql:quickload :galaxians)" \
	     --eval "(galaxians:main)"

test:
	sbcl --eval "(ql:quickload :galaxians)" \
	     --eval "(unless (fiveam:run-all-tests) (uiop:quit 1))" \
	     --eval "(uiop:quit 0)"
