package ADT;
import java.util.ArrayList;
import java.util.List;

import ADT.IStack;

public class Queue<T> implements IStack<T>{
    private List<T> queue;
    public Queue() {
        queue = new ArrayList<T>();
    }

    public void push(T item) {
        queue.add(item);
    }

    public T pop() {
        return queue.remove(0);
    }

    public T peek() {
        return queue.get(0);
    }
}
