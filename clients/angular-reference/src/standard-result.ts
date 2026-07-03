export interface StandardProblem {
  code: string;
  message: string;
  target?: string | null;
}

export interface StandardResult<TValue> {
  succeeded: boolean;
  value?: TValue | null;
  problem?: StandardProblem | null;
}

export interface StandardQuery {
  pageNumber: number;
  pageSize: number;
  searchText?: string | null;
}

export interface StandardCollectionResult<TItem> {
  items: TItem[];
  totalCount: number;
  query: StandardQuery;
}

export function isSuccessful<TValue>(result: StandardResult<TValue>): boolean {
  return result.succeeded === true;
}
