venv/.completed: requirements.txt Makefile
	python3 -m venv venv
	venv/bin/pip install -r requirements.txt
	touch venv/.completed
