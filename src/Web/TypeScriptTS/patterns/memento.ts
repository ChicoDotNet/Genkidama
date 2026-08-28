function mementoPattern(){let state='draft';const snapshot=state;state='published';state=snapshot;return state==='draft'}
