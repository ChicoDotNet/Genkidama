import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { StandardApiClient } from './standard-api-client';
import { StandardResultQueryState, completedState, loadingState } from './standard-result-query';

interface DashboardSummary {
  title: string;
  totalItems: number;
}

@Component({
  selector: 'genkidama-example-dashboard',
  standalone: true,
  imports: [CommonModule],
  template: `
    <p *ngIf="state.loading">Loading...</p>
    <p *ngIf="state.problem">{{ state.problem.message }}</p>
    <section *ngIf="state.value">
      <h1>{{ state.value.title }}</h1>
      <p>Total items: {{ state.value.totalItems }}</p>
    </section>
  `
})
export class ExampleDashboardComponent implements OnInit {
  protected state: StandardResultQueryState<DashboardSummary> = loadingState();
  private readonly client = inject(StandardApiClient);

  public ngOnInit(): void {
    this.client
      .getResult<DashboardSummary>('dashboard/summary')
      .subscribe(result => this.state = completedState(result));
  }
}
