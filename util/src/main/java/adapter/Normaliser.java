package adapter;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Normaliser<T> {

    public static abstract class Adapter<T> {

        public abstract LocalDate getBegin();

        public abstract LocalDate getEnd();

        public abstract void setEnd(LocalDate end);

        public abstract void setActive(Boolean active);

        private final T adaptee;

        public Adapter(T adaptee) {
            this.adaptee = adaptee;
        }

        protected Adapter<T> withEnd(LocalDate end) {
            setEnd(end);
            return this;
        }

        protected Adapter<T> withActive(Boolean active) {
            setActive(active);
            return this;
        }

        public T unwrap() {
            return adaptee;
        }
    }

    private final List<Adapter<T>> adapters;

    public Normaliser(List<Adapter<T>> adapters) {
        this.adapters = adapters;
    }

    private Adapter<T> activateOn(Adapter<T> adapter, LocalDate date) {
        Boolean before = adapter.getBegin().isEqual(date) || adapter.getBegin().isBefore(date);
        Boolean after  = adapter.getEnd() != null && adapter.getEnd().isAfter(date);
        return adapter.withActive(before && after);
    }

    public List<T> normalise(LocalDate date) {

        if (adapters.isEmpty()) {
            return new ArrayList<>();
        }

        List<Adapter<T>> ordered =
            adapters
                .stream()
                .sorted(Comparator.comparing(Adapter::getBegin))
                .toList();

        List<Adapter<T>> periodic =
            IntStream
                .range(0, ordered.size() - 1)
                .mapToObj(i -> ordered.get(i).withEnd(ordered.get(i + 1).getBegin()))
                .collect(Collectors.toList());

        periodic.add(ordered.getLast());

        return
            periodic
                .stream()
                .map(a -> activateOn(a, date))
                .map(Adapter::unwrap)
                .toList();
    }
}
