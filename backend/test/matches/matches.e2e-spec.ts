import { INestApplication, ValidationPipe } from "@nestjs/common";
import { Test } from "@nestjs/testing";
import { AppModule } from "src/app.module";
import { PrismaService } from "src/prisma/prisma.service";

const runMatchesTests = (pactum: any) => {
  let app: INestApplication;
  let prisma: PrismaService;

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
    prisma = app.get(PrismaService);
  });

  describe("Matches", () => {

    it(`/matches (GET) no pagination`, () => {
      return pactum
        .spec()
        .get(`/matches`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it(`/matches (GET) pagination`, () => {
      return pactum
        .spec()
        .get(`/matches?page=1&pageSize=3`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
    });

    it(`/matches (GET) filtering bad`, () => {
      return pactum
        .spec()
        .get(
          `/matches?page=1&pageSize=3&filterOn=avatar,status&filterBy=asdf.avatar.jpg,OFFLINE`
        )
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400);
    });

    it(`/matches (GET) filtering`, async () => {
      const currUser = await prisma.user.findFirst({
        where: {
          username: "new",
        },
      });

      return pactum
        .spec()
        .get(
          `/matches?page=1&pageSize=3&filterOn=playerId1,winnerId&filterBy=${currUser.id},${currUser.id}`
        )
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
    });

    it(`/matches (GET) filtering 2`, async () => {
      const currUser = await prisma.user.findFirst({
        where: {
          username: "new",
        },
      });
      return pactum
        .spec()
        .get(
          `/matches?page=1&pageSize=3&filterOn=playerId1,playerId0&filterBy=${currUser.id},${currUser.id}&operator=OR`
        )
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
    });
  });
};

export default runMatchesTests;
