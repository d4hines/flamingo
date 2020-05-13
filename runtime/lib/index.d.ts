declare module "flamingo-runtime" {
    export interface FlamingoObject {
        /** A string representing type of object or action. */
        type: string;
        /** An Javascript object where the keys are are attributes
         * of the Flamingo object and the values are the values of
         * those attributes.
         */
        payload: any;
    }
    export type FlamingoStateChange = {
        /** A string representing name of the fluent whose value changed. */
        type: string;
        /** The values of that fluent. */
        value: (string | number)[];
        /** The operation performed (insert or delete). 1 means the fact
         * was inserted (i.e, the fact became true in the model as a result
         * of the action). -1 means the fact was deleted (i.e, it was true,
         * but became false as a result of the action).
        */
        op: -1 | 1;
    }
    /**
     * A JS handle to a Flamingo model. Extends the EventEmitter
     * class, allowing users to listen for changes to the model's
     * state.
     * 
     * @example 
     * const flamingo = new Flamingo();
     * flamingo.on('some_output_fluent', console.log);
     */
    export class Flamingo extends import("events").EventEmitter {
        constructor(): void;
        /**
         * Adds an object to Flamingo's model.
         * @param object The object to be added.
         * @returns The added object's unique id in the model.
         * This ID can then be used to refer to that object
         * when creating other objects or actions.
         */
        add(object: FlamingoObject): number;
        /**
         * Dispatches an action to Flamingo's model, synchronously
         * returning the changes to the model's state.
         * @param action 
         */
        dispatch(action: FlamingoObject): FlamingoStateChange;
        /**
         * Stops the Flamingo engine, destroying its state and
         * releasing its memory.
         */
        stop(): void;
    }
}
