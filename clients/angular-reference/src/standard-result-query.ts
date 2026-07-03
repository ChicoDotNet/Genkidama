import type { StandardProblem, StandardResult } from './standard-result';

export interface StandardResultQueryState<TValue> {
  loading: boolean;
  value?: TValue | null;
  problem?: StandardProblem | null;
}

export function loadingState<TValue>(): StandardResultQueryState<TValue> {
  return { loading: true };
}

export function completedState<TValue>(
  result: StandardResult<TValue>
): StandardResultQueryState<TValue> {
  return {
    loading: false,
    value: result.value,
    problem: result.problem
  };
}
