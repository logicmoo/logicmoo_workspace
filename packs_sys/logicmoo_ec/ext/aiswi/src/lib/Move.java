package lib;

/**
 * Rappresenta una mossa
 * 
 * @author Baioni, Grandi, Tallevi Diotallevi
 */
public class Move {
    private int x, y;
    String color;
    String figure;
    int time;

    /**
     *
     * @param x
     * @param y
     * @param color
     * @param figure
     * @param time
     */
    public Move(int x, int y, String color, String figure, int time) {
        this.x = x;
        this.y = y;
        this.color = color;
        this.figure = figure;
        this.time = time;
    }


    /**
     * Get X
     * @return
     */
    public int getX(){
        return this.x;
    }

    /**
     * Get Y
     * @return
     */
    public int getY(){
        return this.y;
    }

    /**
     * Get time
     * @return
     */
    public int getTime(){
        return this.time;
    }

    @Override
    public String toString() {
        if (x<0 || y<0){
            return "hap(sono(A,B,C,D), "+ time + ".0).";
        }else{
            return "hap(sono(" + x + "," + y + "," + color + "," + figure + "), " + time + ".0).";
        }
    }

}
