package net.sf.regulus;


public class RegClientException extends Exception {
    String reason = null;

    /**
     * @param string
     */
    public RegClientException(String reason) {
        this.reason = reason;
    }

    /**
     * @return Returns the reason.
     */
    public String getReason() {
        return reason;
    }
}
