package lib;

import gamegui.Cell;
import gamegui.ChessBoard;

/**
 *
 * @author Baioni, Grandi, Tallevi Diotallevi
 */
public class WorldDescription {

    /**
     * Generazione di una descrizione del mondo a partire della matrice di celle
     * @param cells
     * @return
     */
    public static String generateWorldDescription(Cell[][] cells) {
         Cell[][] c = cells;

        String world = "";
        for (int i = 0; i < ChessBoard.DIM; i++) {
            for (int j = 0; j < ChessBoard.DIM; j++) {
                String figure = c[i][j].getFigure();
                String color = c[i][j].getColor();
                String pos_x = "" + c[i][j].getPosX();
                String pos_y = "" + c[i][j].getPosY();
                String element = "cell("
                        + pos_x + ","
                        + pos_y + ","
                        + color + ","
                        + figure
                        + ").\n";
                world += element;
            }
        }
      return world;

    }
  

       

}
