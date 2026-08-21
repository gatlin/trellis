class Observable {
    _action;
    constructor(_action) {
        this._action = _action;
    }
    subscribe(observer) {
        const finish = "function" === typeof observer
            ? this._action({
                next(v) {
                    return observer(v);
                }
            })
            : this._action(observer);
        return !finish
            ? {
                finish() {
                    return;
                }
            }
            : "function" === typeof finish
                ? { finish }
                : finish;
    }
}
function pure(value) {
    return new Observable((observer) => observer.next(value));
}
function shift(fn) {
    return new Observable((observer) => fn((value) => observer.next(value)));
}
function each(it) {
    return new Observable((observer) => {
        let cancelled = false;
        void setTimeout(() => {
            for (const value of it) {
                if (cancelled) {
                    break;
                }
                else {
                    observer.next(value);
                }
            }
        }, 0);
        return () => {
            cancelled = true;
        };
    });
}
function keep(promise) {
    return new Observable((observer) => {
        let cancelled = false;
        promise.then((value) => {
            if (!cancelled) {
                observer.next(value);
            }
        });
        return () => {
            cancelled = true;
        };
    });
}
function par(observables) {
    return new Observable((observer) => {
        const activities = [];
        for (const observable of observables) {
            setTimeout(() => {
                const activity = observable.subscribe(observer);
                activities.push(activity);
            }, 0);
        }
        return () => {
            activities.slice().forEach((activity) => void activity.finish());
        };
    });
}
function map(fn) {
    return (source) => shift((observer) => source.subscribe({
        count: 0,
        next(value) {
            return observer(fn(value, this.count++));
        }
    }));
}
function join() {
    return (source) => shift((observer) => source.subscribe((value) => value.subscribe(observer)));
}
function then(fn) {
    return (source) => join()(map(fn)(source));
}
function filter(fn) {
    return (source) => shift((observer) => source.subscribe({
        count: 0,
        next(value) {
            if (fn(value, this.count++)) {
                return observer(value);
            }
        }
    }));
}
function prune() {
    return (source) => shift((observer) => {
        const activity = source.subscribe((value) => {
            activity.finish();
            return observer(value);
        });
        return activity;
    });
}
function reset(observable) {
    const pruned = prune()(observable);
    return new Promise((resolve) => void pruned.subscribe(resolve));
}
class Wire extends Observable {
    _done = false;
    _subscribers = [];
    constructor() {
        super((observer) => {
            if (!this.done) {
                this._subscribers.push(observer);
                return () => {
                    for (const [index, subscriber] of this._subscribers.entries()) {
                        if (subscriber === observer) {
                            this._subscribers.splice(index, 1);
                            break;
                        }
                    }
                };
            }
        });
    }
    get done() {
        return this._done;
    }
    finish() {
        this._subscribers = [];
        this._done = true;
        return;
    }
    next(value) {
        if (!this.done) {
            this._subscribers
                .slice()
                .forEach((cont) => cont.next(value));
        }
        return;
    }
}
class Behavior extends Wire {
    _value;
    constructor(_value) {
        super();
        this._value = _value;
    }
    get value() {
        return this._value;
    }
    subscribe(observer) {
        const activity = super.subscribe(observer);
        "function" === typeof observer
            ? observer(this.value)
            : observer.next(this.value);
        return activity;
    }
    next(newValue) {
        if (!this.done) {
            super.next((this._value = newValue));
        }
        return;
    }
}
export { Observable, pure, shift, each, par, keep, Wire, Behavior, map, join, filter, then, prune, reset };
//# sourceMappingURL=index.js.map