interface Activity {
    finish(): void;
}
interface Observer<A, B = void> {
    next(value?: A): B;
}
declare class Observable<A> {
    private _action;
    constructor(_action: (c: Observer<A, unknown>) => Activity | (() => void) | void);
    subscribe<B = void>(observer: Observer<A, B> | ((a: A) => B)): Activity;
}
declare function pure<A>(value?: A): Observable<A>;
declare function shift<A>(fn: (k: (value: A) => unknown) => void): Observable<A>;
declare function each<A>(it: Iterable<A>): Observable<A>;
declare function keep<A>(promise: PromiseLike<A>): Observable<A>;
declare function par<A>(observables: Iterable<Observable<A>>): Observable<A>;
interface Transformer<A, B> {
    (s: Observable<A>): Observable<B>;
}
declare function map<A, B>(fn: (value: A, index?: number) => B): Transformer<A, B>;
declare function join<A>(): Transformer<Observable<A>, A>;
declare function then<A, B>(fn: (a: A) => Observable<B>): Transformer<A, B>;
declare function filter<A>(fn: (value: A, index?: number) => boolean): Transformer<A, A>;
declare function prune<A>(): Transformer<A, A>;
declare function reset<A>(observable: Observable<A>): Promise<A>;
declare class Wire<A> extends Observable<A> implements Activity, Observer<A> {
    protected _done: boolean;
    protected _subscribers: Observer<A, unknown>[];
    constructor();
    get done(): boolean;
    finish(): void;
    next(value: A): void;
}
declare class Behavior<A> extends Wire<A> implements Activity, Observer<A> {
    protected _value: A;
    constructor(_value: A);
    get value(): A;
    subscribe(observer: Observer<A> | ((a: A) => unknown)): Activity;
    next(newValue: A): unknown;
}
export { Observable, pure, shift, each, par, keep, Wire, Behavior, map, join, filter, then, prune, reset };
export type { Activity, Observer, Transformer };
