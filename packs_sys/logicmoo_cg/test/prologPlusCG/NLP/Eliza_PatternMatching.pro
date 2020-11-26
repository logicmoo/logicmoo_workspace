// ELIZA : Simuler un dialogue entre un psychiatre et un patient, basé sur le Pattern-Matching 
// Exemple de dialogue:
// Patient: Je suis toujours en désaccord avec mes parents.
// Medecin: Etes-vous venu me voir parce que vous etes toujours en désaccord avec vos parents ?
// Patient: Je pense que mes parents me detestent.
// Medecin: Développez vos sentiments.
// ...
// - Je ne peux pas vous expliquer mon problème.
// - Est ce que vous avez peur de me raconter ce qui vous angoisse ?
// - Non.
// - Dites-vous non juste pour etre negatif ?
// - Non,
// - Vous etes un peu pessimiste !
// - vous etes génial.
// - Qu'est-ce qui vous fait croire que je suis genial ?
// ...


// Analyse par pattern-matching utilise un dictionnaire de patterns: chaque entrée d'un tel dictionnaire est un
// couple dont le premier élément est un pattern d'entrée et le second élément une liste de patterns de sortie,
// si la phrase en entrée match/s'apparie au pattern d'entrée, un des patterns en sortie sera choisie comme réponse
// Notons qu'en général, les deux patterns E/S partagent des variables qui simulent l'esprit d'un dialogue ...

dict([y, "je", "ne", "peux", "pas", x, "problème", z, "."],
     [["est ce que vous avez peur de me raconter ce qui vous angoisse ?"],
      ["peut être vous vous sentez angoissé de ", x, " problème", z]]).
dict([y, "je", "ne", "peux", "pas", x],
     [["vous n'aimez vraiment pas ", x],
      ["pourquoi ne voulez-vous pas ", x],
      ["souhaitez-vous pouvoir ", x],
      ["est-ce que cela vous ennuit"]]).
dict([y, "pere", x, "."],
     [["parlez-moi plus de votre famille"],
      ["qui encore dans votre famille", x]]).
dict([y, "mere", x, "."],
     [["parlez-moi plus de votre famille"],
      ["qui encore dans votre famille", x]]).
// ...
dict([x, "."],
     [["pouvez-vous etre plus explicite sur ce qui vous effraie ?"],
      ["qu'est-ce que cela vous suggere ?"],
      ["je vois"],
      ["je ne suis pas sur de bien vous comprendre"],
      ["continuez, soyez plus explicite"],
      ["pouvez-vous développer ce point ?"],
      ["C'est très interessant"]]).

// dict de certaines particules linguistiques
// dans un dialogue, on peut avoir: 
// - je te regarde
// - pourquoi me regarde tu ?
transforme("vous", "je").
transforme("etes", "suis").
transforme("suis", "etes").
// ...
transforme("me", "vous").
transforme("te", "me").

// Le but patternMatch est le coeur de l'application:
// TantQue la phrase en entrée n'est pas "bye", faire:
// 1- chercher une entrée dans le dict des patterns:
// 2- determine ensuite le rang de l'entrée en sortie à activer
// 3- Afficher le pattern en sortie choisie en considérant les transformations des particules linguistiques
// 4- On m-à-j le rang afin de choisir la prochaine sortie 

// Le but match(phrase, pattern) est une variante/extension de l'unification de Prolog; il est défini comme suit:
// 1- si on arrive au dernier élément du pattern et si on peut l'unifier avec le reste de la phrase, le matching 
//    se terminera alors avec succès.
// 2- si l'élément courant du pattern est une constante et qu'il est équivalent à l'élément courant de la phrase,
//    on poursuit alors le matching avec le reste de la phrase et du pattern.
// 3- si l'élément courant du pattern est une variable, l'élément courant doit s'instancier à la sous-liste de la //    phrase; cette sous-liste étant délimitée par l'élément suivant dans le pattern. On poursuit ensuite le
//    matching avec le reste de la phrase et du pattern.
// 4- Si aucun des cas précédents ne réussit, conclure l'échec du matching