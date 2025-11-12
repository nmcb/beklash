package adapter;

import java.time.LocalDate;

public class Bean {
    
    private LocalDate begin;
    private LocalDate end;
    private Boolean active;

    @Override
    public String toString() {
        return "Bean{" +
                "begin=" + begin +
                ", end=" + end +
                ", active=" + active +
                '}';
    }

    public Bean(LocalDate begin) {
        this.begin = begin;
    }

    public LocalDate getBegin() {
        return begin;
    }

    public void setBegin(LocalDate begin) {
        this.begin = begin;
    }

    public LocalDate getEnd() {
        return end;
    }

    public void setEnd(LocalDate end) {
        this.end = end;
    }

    public Boolean getActive() {
        return active;
    }

    public void setActive(Boolean active) {
        this.active = active;
    }
}
