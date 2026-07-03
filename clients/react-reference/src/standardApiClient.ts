import type { StandardCollectionResult, StandardProblem, StandardResult } from './standardResult';

export interface StandardApiClientOptions {
  baseUrl: string;
  fetcher?: typeof fetch;
}

export class StandardApiClient {
  private readonly baseUrl: string;
  private readonly fetcher: typeof fetch;

  public constructor(options: StandardApiClientOptions) {
    this.baseUrl = options.baseUrl.replace(/\/$/, '');
    this.fetcher = options.fetcher ?? fetch;
  }

  public async getResult<TValue>(path: string): Promise<StandardResult<TValue>> {
    const response = await this.fetcher(this.createUrl(path));
    return this.readResult<TValue>(response);
  }

  public async getCollection<TItem>(path: string): Promise<StandardCollectionResult<TItem>> {
    const result = await this.getResult<StandardCollectionResult<TItem>>(path);
    if (result.succeeded && result.value) return result.value;
    throw new Error(result.problem?.message ?? 'Request failed.');
  }

  private createUrl(path: string): string {
    return `${this.baseUrl}/${path.replace(/^\//, '')}`;
  }

  private async readResult<TValue>(response: Response): Promise<StandardResult<TValue>> {
    if (response.ok) return response.json() as Promise<StandardResult<TValue>>;
    return this.failedResult<TValue>(response.statusText);
  }

  private failedResult<TValue>(message: string): StandardResult<TValue> {
    const problem: StandardProblem = { code: 'http_error', message };
    return { succeeded: false, problem };
  }
}
