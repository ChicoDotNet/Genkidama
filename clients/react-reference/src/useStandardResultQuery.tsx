import { useEffect, useState } from 'react';
import type { StandardProblem, StandardResult } from './standardResult';
import type { StandardApiClient } from './standardApiClient';

export interface StandardResultQueryState<TValue> {
  loading: boolean;
  value?: TValue | null;
  problem?: StandardProblem | null;
}

export function useStandardResultQuery<TValue>(
  client: StandardApiClient,
  path: string
): StandardResultQueryState<TValue> {
  const [state, setState] = useState<StandardResultQueryState<TValue>>({ loading: true });

  useEffect(() => {
    let active = true;
    client.getResult<TValue>(path).then(result => updateState(result, active, setState));
    return () => {
      active = false;
    };
  }, [client, path]);

  return state;
}

function updateState<TValue>(
  result: StandardResult<TValue>,
  active: boolean,
  setState: (state: StandardResultQueryState<TValue>) => void
): void {
  if (!active) return;
  setState({ loading: false, value: result.value, problem: result.problem });
}
