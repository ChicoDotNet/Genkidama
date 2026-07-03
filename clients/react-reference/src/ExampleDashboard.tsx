import { useMemo } from 'react';
import { StandardApiClient } from './standardApiClient';
import { useStandardResultQuery } from './useStandardResultQuery';

interface DashboardSummary {
  title: string;
  totalItems: number;
}

export function ExampleDashboard(): JSX.Element {
  const client = useMemo(() => new StandardApiClient({ baseUrl: '/api' }), []);
  const state = useStandardResultQuery<DashboardSummary>(client, 'dashboard/summary');

  if (state.loading) return <p>Loading...</p>;
  if (state.problem) return <p>{state.problem.message}</p>;

  return (
    <section>
      <h1>{state.value?.title ?? 'Dashboard'}</h1>
      <p>Total items: {state.value?.totalItems ?? 0}</p>
    </section>
  );
}
