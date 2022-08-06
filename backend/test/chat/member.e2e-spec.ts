import { INestApplication, ValidationPipe } from "@nestjs/common";
import { Test } from "@nestjs/testing";
import { AppModule } from "src/app.module";
import { PrismaService } from "src/prisma/prisma.service";

const runMemberTests = (pactum: any) => {
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

  describe("Members", () => {
    it("/members (POST) bad dto", () => {
      const dto = {
        intraName: "wtf",
      };

      return pactum
        .spec()
        .post(`/members`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/members (POST) bad dto 2", () => {
      const dto = {
        roomId: -1,
      };

      return pactum
        .spec()
        .post(`/members`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(404)
        .expectBodyContains("error");
    });

    it("/members (POST) bad dto 3", () => {
      const dto = {
        roomId: -1,
      };

      return pactum
        .spec()
        .post(`/members`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(404)
        .expectBodyContains("error");
    });

    it("/members (POST) invalid password", async () => {
      const room = await prisma.room.findFirst({
        where: {
          isProtected: true,
        },
      });
      const dto = {
        roomId: room.id,
      };

      return pactum
        .spec()
        .post(`/members`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(401)
        .expectBodyContains("error");
    });

    it("/members (POST) invalid password 2", async () => {
      const room = await prisma.room.findFirst({
        where: {
          isProtected: true,
        },
      });
      const dto = {
        roomId: room.id,
        password: "1234",
      };

      return pactum
        .spec()
        .post(`/members`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(401)
        .expectBodyContains("error");
    });

    it("/members (POST) already join", async () => {
      const room = await prisma.room.findFirst({
        where: {
          isProtected: true,
        },
      });
      const dto = {
        roomId: room.id,
        password: "passw0rd!",
      };

      return pactum
        .spec()
        .post(`/members`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it(`/members (GET) no pagination`, () => {
      return pactum
        .spec()
        .get(`/members`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it(`/members (GET) pagination`, () => {
      return pactum
        .spec()
        .get(`/members?page=1&pageSize=3`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectJsonLength("data", 3);
    });

    it(`/members (GET) filtering bad`, () => {
      return pactum
        .spec()
        .get(
          `/members?page=1&pageSize=3&filterOn=avatar,status&filterBy=asdf.avatar.jpg,OFFLINE`
        )
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400);
    });

    it(`/members (GET) filtering`, () => {
      return pactum
        .spec()
        .get(`/members?page=1&pageSize=3&filterOn=roomId,userId&filterBy=-1,-1`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectJsonLength("data", 0);
    });

    it(`/members (GET) filtering 2`, async () => {
      const user = await prisma.user.findFirst({
        where: {
          username : "new"
        },
      });
      return pactum
        .spec()
        .get(`/members?page=1&pageSize=3&filterOn=userId&filterBy=${user.id}`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectJsonLength("data", 2);
    });

    it("/members (POST)", async () => {
      const room = await prisma.room.findFirst({
        where: {
          isProtected: true,
        },
      });
      const user = await prisma.user.findFirst({
        where: {
          username : "new"
        },
      });
      await prisma.member.deleteMany({
        where: {
          userId: user.id,
          roomId: room.id,
        },
      });
      const dto = {
        roomId: room.id,
        password: "passw0rd!",
      };

      return pactum
        .spec()
        .post(`/members`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(201);
    });

    it("/members (DELETE) bad id", async () => {
      return pactum
        .spec()
        .delete(`/members/-1`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(404)
        .expectBodyContains("error");
    });

    it("/members (DELETE) leave owner", async () => {
      const room = await prisma.room.findFirst({
        where: {
          isProtected: true,
        },
      });
      const user = await prisma.user.findFirst({
        where: {
          username : "new"
        },
      });
      await prisma.room.update({
        where: { id: room.id },
        data: { ownerId: user.id },
      });
      const member = await prisma.member.findFirst({
        where: { userId: user.id, roomId: room.id },
      });

      return pactum
        .spec()
        .delete(`/members/${member.id}`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/members (DELETE) leave", async () => {
      const room = await prisma.room.findFirst({
        where: {
          isProtected: true,
        },
      });
      const user = await prisma.user.findFirst({
        where: {
          intraName: "tes",
        },
      });
      await prisma.room.update({
        where: { id: room.id },
        data: { ownerId: user.id },
      });
      const member = await prisma.member.findFirst({
        where: { userId: user.id, roomId: room.id },
      });

      return pactum
        .spec()
        .delete(`/members/${member.id}`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200);
    });
  });
};

export default runMemberTests;
