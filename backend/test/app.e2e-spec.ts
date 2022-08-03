import { Test } from "@nestjs/testing";
import { INestApplication, ValidationPipe } from "@nestjs/common";
import { AppModule } from "./../src/app.module";
import * as pactum from "pactum";
import { PrismaService } from "src/prisma/prisma.service";
import runUserTests from "./user/user.e2e-spec";
import runFriendsTests from "./friends/friends.e2e-spec";
import runBlocksTests from "./blocks/blocks.e2e-spec";
import runOTPTests from "./otp/otp.e2e-spec";

describe("App (e2e)", () => {
  let app: INestApplication;
  let prisma: PrismaService;

  // eslint-disable-next-line @typescript-eslint/no-var-requires
  const prompt = require("prompt-sync")({ sigint: true });
  const confirm = prompt(
    "This will wipe ALL data in the current database. enter Y to proceed"
  );
  if (confirm != "Y") return;

  const code = prompt(
    "Please provide the 42 auth code used for authentication"
  );

  //run this before all testcases
  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();
    app = moduleRef.createNestApplication();
    app.useGlobalPipes(
      new ValidationPipe({
        whitelist: true,
      })
    );
    await app.init();
    await app.listen(3333);
    prisma = app.get(PrismaService);
    await prisma.cleanDb();
    await prisma.seedSampleUsers();
    pactum.request.setBaseUrl("http://localhost:3001/api/v1");
  });

  it("/ (GET)", () => {
    return pactum.spec().get("/").expectStatus(200);
  });

  describe("Auth", () => {
    it(`/auth/authenticate?code=12 (GET)`, () => {
      return pactum
        .spec()
        .get(`/auth/authenticate?code=12`)
        .expectStatus(401)
        .expectBodyContains("error")
        .expectBodyContains("Unauthorized");
    });

    it(`/auth/authenticate?code=${code} (GET)`, () => {
      return pactum
        .spec()
        .get(`/auth/authenticate?code=${code}`)
        .expectStatus(200)
        .expectBodyContains("token")
        .expectBodyContains("expiresAt")
        .stores("token", "data.token");
    });

    it(`/auth/authenticate?code=${code} (GET)`, () => {
      return pactum
        .spec()
        .get(`/auth/authenticate?code=${code}`)
        .expectStatus(401)
        .expectBodyContains("error")
        .expectBodyContains("Unauthorized");
    });

    it(`/auth/logout (GET) (bad token)`, () => {
      return pactum
        .spec()
        .get(`/auth/logout`)
        .withHeaders({
          Authorization: "Bearer ererer",
        })
        .expectStatus(403);
    });
  });

  runUserTests(pactum);
  runFriendsTests(pactum);
  runBlocksTests(pactum);
  runOTPTests(pactum);

  describe("Auth logout ", () => {
    it(`/auth/logout (GET)`, () => {
      return pactum
        .spec()
        .get(`/auth/logout`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectBodyContains("token");
    });
  });
  afterAll(() => {
    app.close();
    // prisma.cleanDb();
  });
});
