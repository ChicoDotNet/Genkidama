import { createServer } from "node:http";
import { createAppState, createRequestHandler } from "./app.js";

const port = Number(process.env.PORT ?? 3000);
const server = createServer(createRequestHandler(createAppState()));
server.listen(port, () => {
  console.log(`FreelanceDesk disponible en http://localhost:${port}`);
});
