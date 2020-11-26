Nu är den fin och färdig. Samma funktionalitet som test-programmet,
dock lite annorlunda utseende på termerna (dock samma logiska
innehåll)

Liten OAA-lektion

För att testa:
0. se till att filen setup.pl finns i hembiblioteket

1. skapa lämpligt bibliotek

1,5. prepenv CLASSPATH /users/ling/sl/oaa2.2.0/runtime/oaalib/oaa2.jar

2. packa upp diseasedb.zip där (den som kom med mail)

3. lägg OAAWorkshopDatabase.java i samma bibliotek (den ska vara på samma
nivå som t.ex. README-filen

4. > javac OAAWorkshopDatabase.java

5. starta facilitator
	> cd oaabibliotek/runtime
	> fac.sh

6.  starta debug-agenten
	> cd oaabibliotek/runtime
	> debug.sh

7 starta databasagenten
	cd lämpligt_bibliotek
	java OAAWorkshopDatabase diseasedb.ser

Nu kan man ställa frågor till databasen genom debug-agenten t.ex.
	oaa_Solve(disease_names(A),[])
	oaa_Solve(diagnose([not(symptom(headache)),symptom(cough)],A),[])
	...

solve-knappen ger ett oaa_Solve-predikatskal
agent info-knappen konstruerar en query som frågar facilitatorn vilka solvables
som finns deklarerade för olika agenter...
skicka query genom att trycka på den lilla fyrkanten till höger.


Man kan också testa genom att strunta i att starta debug-agenten och
konsultera oaag.pl-resursen och köra från prolog...

