package adapter;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

public class MainNormaliser {

  public static void main(String[] args) {

    List<Bean> list = new ArrayList<>();
    list.add(new Bean(LocalDate.now()));
    list.add(new Bean(LocalDate.now().plusDays(1)));
    list.add(new Bean(LocalDate.now().plusDays(5)));
    list.add(new Bean(LocalDate.now().plusDays(8)));
    list.add(new Bean(LocalDate.now().plusDays(15)));
    list.forEach(b -> System.out.println(b.toString()));

    
    LocalDate date = LocalDate.now().plusDays(9);
    System.out.println("date: " + date);

    List<Bean> normalized =
      new Normaliser<>(
        list.stream().map(b -> new Normaliser.Adapter<>(b) {
          public LocalDate getBegin() {
            return b.getBegin();
          }

          public LocalDate getEnd() {
            return b.getEnd();
          }

          public void setEnd(LocalDate end) {
            b.setEnd(end);
          }

          public void setActive(Boolean active) {
            b.setActive(active);
          }
        }).toList()
      ).normalise(date);

    normalized.forEach(b -> System.out.println(b.toString()));
  }
}
