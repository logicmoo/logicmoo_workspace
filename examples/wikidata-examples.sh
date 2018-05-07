# WikiData Examples
# ---
# These all use pq-wd in the bin/ directory, which is a one-line wrapper onto pl2sparql,
# which loads the wikidata.pl module and sets the endpoint to the main wikidata endpoint.

# --
# Non-Biology Examples
# --

# continents and their english-language labels
pq-wd  'continent(C),label(C,CN),lang(CN)="en"'

# as above, using convenience predicate
pq-wd  'continent(C),enlabel(C,CN)'

# power stations and their locations
# Note: affixing "_inf" on the end of a class predicate will return inferred classification
pq-wd   "power_station_inf(X),coordinate_location(X,Loc),enlabel(X,XN)"

# cities with populations over 10m, plus the contininent they are part of
pq-wd "city_inf(City),part_of_continent(City,Continent),enlabel(City,CityN),enlabel(Continent,ContinentN),population(City,Pop),Pop>10000000"

# cities in more than one continent
pq-wd    "city_inf(C),part_of_continent(C,X1),part_of_continent(C,X2),X1@>X2,enlabel(C,CN)"

# --
# Biology Examples
# --

# disease-to-gene-to-protein
# this uses enlabel/2, which is the english-language label
pq-wd   "disease(D),genetic_association(D,G),encodes(G,P),enlabel(D,DN),enlabel(P,PN)"

# diseases, causes and treatments
pq-wd   "isa_disease(X),has_cause(X,C),treated_by_drug(X,S),enlabel(X,XN),enlabel(C,CN),enlabel(S,SN)"

# civic variants and their types
pq-wd   "civic_id(V,X),instance_of(V,T),enlabel(V,VN),enlabel(T,TN)"

# variant that is PTP for a drug-condition
# See WD example queries
pq-wd   "V='http://www.wikidata.org/entity/Q25100112', civic_id(V,X),positive_therapeutic_predictor_e2s(V,S),medical_condition_treated_s2q(S,C),positive_therapeutic_predictor_s2v(S,Drug),enlabel(Drug,DrugN),enlabel(V,VN),enlabel(C,CN)" "v2d_cond(V,VN,Drug,DrugN,C,CN)"

# --
# Bio-ontology Examples
# --

# concepts with an exact match to something in SO
pq-wd  'exact_match(X,URI),str_starts(str(URI),"http://purl.obolibrary.org/obo/SO_")'  

# properties mapped to RO
pq-wd  'ro_id(P,X)'

# Get GO to MESH mappings (via shared WD entry)
pq-wd  "mesh_id(X,M),go_id(X,G)"
