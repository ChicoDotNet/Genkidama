import { Injectable, InjectionToken, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, catchError, map, of } from 'rxjs';
import type { StandardCollectionResult, StandardProblem, StandardResult } from './standard-result';

export const GENKIDAMA_API_BASE_URL = new InjectionToken<string>('GENKIDAMA_API_BASE_URL');

@Injectable({ providedIn: 'root' })
export class StandardApiClient {
  private readonly http = inject(HttpClient);
  private readonly baseUrl = inject(GENKIDAMA_API_BASE_URL, { optional: true }) ?? '/api';

  public getResult<TValue>(path: string): Observable<StandardResult<TValue>> {
    return this.http.get<StandardResult<TValue>>(this.createUrl(path)).pipe(
      catchError(error => of(this.failedResult<TValue>(error?.message ?? 'Request failed.')))
    );
  }

  public getCollection<TItem>(path: string): Observable<StandardCollectionResult<TItem> | null> {
    return this.getResult<StandardCollectionResult<TItem>>(path).pipe(
      map(result => result.succeeded ? result.value ?? null : null)
    );
  }

  private createUrl(path: string): string {
    return `${this.baseUrl.replace(/\/$/, '')}/${path.replace(/^\//, '')}`;
  }

  private failedResult<TValue>(message: string): StandardResult<TValue> {
    const problem: StandardProblem = { code: 'http_error', message };
    return { succeeded: false, problem };
  }
}
