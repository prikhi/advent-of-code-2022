.PHONY: run watch compile watch-run new-day clean

new-day:
	./mk-day.sh "$(day)"

compile:
	ghc -dynamic -threaded -rtsopts -O2 -Wall -main-is Day$(day) Day$(day).hs -odir build -hidir build -o run
	./run < inputs/day$(day).txt
	rm -f run

watch:
	ls *.hs Data/* inputs/* | entr make compile day=$(day)

run:
	runghc --ghc-arg=-Wall Day$(day).hs < inputs/day$(day).txt

watch-run:
	ls *.hs Data/* inputs/* | entr make run day=$(day)

clean:
	rm -rf run build
