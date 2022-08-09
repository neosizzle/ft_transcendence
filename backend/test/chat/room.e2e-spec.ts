import { INestApplication, ValidationPipe } from "@nestjs/common";
import { Test } from "@nestjs/testing";
import { AppModule } from "src/app.module";
import { PrismaService } from "src/prisma/prisma.service";

const runRoomTests = (pactum: any) => {
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

  describe("Rooms", () => {
    it("/rooms (POST) bad dto", () => {
      const dto = {
        intraName: "wtf",
      };

      return pactum
        .spec()
        .post(`/rooms`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (POST) bad dto 2", async () => {
      const sampleUser = await prisma.user.findFirst({
        where: { username: "tes" },
      });
      const sampleUserTwo = await prisma.user.findFirst({
        where: { username: "test2" },
      });
      const dto = {
        password: "1234",
        type: "GC",
        initialUsers: `${sampleUser.id},${sampleUserTwo.id}`,
      };

      return pactum
        .spec()
        .post(`/rooms`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (POST) bad dto 3", async () => {
      const sampleUser = await prisma.user.findFirst({
        where: { username: "tes" },
      });
      const sampleUserTwo = await prisma.user.findFirst({
        where: { username: "test2" },
      });
      const dto = {
        password: "1213123123234",
        type: "GC",
        initialUsers: `${sampleUser.id},${sampleUserTwo.id}`,
      };

      return pactum
        .spec()
        .post(`/rooms`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (POST) bad dto 4", async () => {
      const sampleUser = await prisma.user.findFirst({
        where: { username: "tes" },
      });
      const sampleUserTwo = await prisma.user.findFirst({
        where: { username: "test2" },
      });
      const dto = {
        roomName: "someroom",
        password: "1213123123234",
        type: "GCsdsd",
        initialUsers: `${sampleUser.id},${sampleUserTwo.id}`,
      };

      return pactum
        .spec()
        .post(`/rooms`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (POST) GC", async () => {
      const sampleUser = await prisma.user.findFirst({
        where: { username: "tes" },
      });
      const sampleUserTwo = await prisma.user.findFirst({
        where: { username: "test2" },
      });
      const dto = {
        roomName: "someroom",
        password: "1213123123234",
        type: "GC",
        initialUsers: `${sampleUser.id},${sampleUserTwo.id}`,
      };

      return pactum
        .spec()
        .post(`/rooms`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(201);
    });

    it("/rooms (POST) bad dto 5", async () => {
      const sampleUser = await prisma.user.findFirst({
        where: { username: "tes" },
      });
      const sampleUserTwo = await prisma.user.findFirst({
        where: { username: "test2" },
      });
      const dto = {
        roomName: "someroom",
        password: "1213123123234",
        type: "DM",
        initialUsers: `${sampleUser.id},${sampleUserTwo.id}`,
      };

      return pactum
        .spec()
        .post(`/rooms`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (POST) bad dto 6", async () => {
      const sampleUser = await prisma.user.findFirst({
        where: { username: "tes" },
      });
      const sampleUserTwo = await prisma.user.findFirst({
        where: { username: "test2" },
      });
      const dto = {
        password: "1213123123234",
        type: "DM",
        initialUsers: `${sampleUser.id},${sampleUserTwo.id}`,
      };

      return pactum
        .spec()
        .post(`/rooms`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (POST) bad dto 7", async () => {
      const sampleUser = await prisma.user.findFirst({
        where: { username: "tes" },
      });
      const sampleUserTwo = await prisma.user.findFirst({
        where: { username: "test2" },
      });
      const dto = {
        roomName: "123214",
        type: "DM",
        initialUsers: `${sampleUser.id},${sampleUserTwo.id}`,
      };

      return pactum
        .spec()
        .post(`/rooms`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (POST) bad dto 8", async () => {
      const sampleUser = await prisma.user.findFirst({
        where: { username: "tes" },
      });
      const sampleUserTwo = await prisma.user.findFirst({
        where: { username: "test2" },
      });
      const dto = {
        type: "DM",
        initialUsers: `${sampleUser.id},${sampleUserTwo.id}`,
      };

      return pactum
        .spec()
        .post(`/rooms`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (POST) DM", async () => {
      const sampleUser = await prisma.user.findFirst({
        where: { username: "te4s" },
      });
      const dto = {
        type: "DM",
        initialUsers: `${sampleUser.id}`,
      };

      return pactum
        .spec()
        .post(`/rooms`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(201);
    });

    it("/rooms (GET) no pagination", async () => {
      return pactum
        .spec()
        .get(`/rooms`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (GET) pagination", async () => {
      return pactum
        .spec()
        .get(`/rooms?page=1&pageSize=3`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectBodyContains("someroom");
    });

    it("/rooms (GET) filtering", async () => {
      return pactum
        .spec()
        .get(`/rooms?page=1&pageSize=3&filterOn=roomName&filterBy=someroom`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectBodyContains("someroom")
        .expectJsonLength("data", 1);
    });

    it("/rooms (GET) filtering bad", async () => {
      return pactum
        .spec()
        .get(`/rooms?page=1&pageSize=3&filterOn=roomNam34e&filterBy=someroom`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400);
    });

    it("/rooms (PATCH) bad dto", async () => {
      const room = await prisma.room.findFirst({
        where: { type: "DM" },
      });
      const dto = {
        isProtedcted: false,
        roomName: "asdf",
      };

      return pactum
        .spec()
        .patch(`/rooms/${room.id}`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (PATCH) bad dto 2", async () => {
      const room = await prisma.room.findFirst({
        where: { type: "GC" },
      });
      const dto = {
        password: "2134",
      };

      return pactum
        .spec()
        .patch(`/rooms/${room.id}`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (PATCH) bad dto 3", async () => {
      const room = await prisma.room.findFirst({
        where: { type: "GC" },
      });
      const dto = {
        isProtected: true,
      };

      return pactum
        .spec()
        .patch(`/rooms/${room.id}`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/rooms (PATCH)", async () => {
      const room = await prisma.room.findFirst({
        where: { type: "GC" },
      });
      const dto = {
        password: "passw0rd!",
      };

      return pactum
        .spec()
        .patch(`/rooms/${room.id}`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200);
    });

    it("/rooms (PATCH) transfer bad owner", async () => {
      const room = await prisma.room.findFirst({
        where: { type: "GC" },
      });
      const user = await prisma.user.findFirst({
        where: { username: "te4s" },
      });
      const dto = {
        ownerId: user.id,
      };

      return pactum
        .spec()
        .patch(`/rooms/${room.id}`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(404)
        .expectBodyContains("error");
    });

    it("/rooms (PATCH) transfer owner", async () => {
      const room = await prisma.room.findFirst({
        where: { type: "GC" },
      });
      const user = await prisma.user.findFirst({
        where: { username: "tes" },
      });
      const dto = {
        ownerId: user.id,
      };

      return pactum
        .spec()
        .patch(`/rooms/${room.id}`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200);
    });

    it("/rooms (PATCH) no permission", async () => {
      const room = await prisma.room.findFirst({
        where: { type: "GC" },
      });
      const user = await prisma.user.findFirst({
        where: { username: "tes" },
      });
      const dto = {
        ownerId: user.id,
      };

      return pactum
        .spec()
        .patch(`/rooms/${room.id}`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(401)
        .expectBodyContains("error");
    });
  });
};

export default runRoomTests;
