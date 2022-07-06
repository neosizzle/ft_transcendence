import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication, ValidationPipe  } from '@nestjs/common';
import * as request from 'supertest';
import { AppModule } from './../src/app.module';
import * as pactum from 'pactum'
import { PrismaService } from 'src/prisma/prisma.service';
import { exit } from 'process';
import { UserPatchDto } from 'src/users/dto';

describe('App (e2e)', () => {
  let app: INestApplication;
  let prisma: PrismaService;

  // eslint-disable-next-line @typescript-eslint/no-var-requires
  const prompt = require("prompt-sync")({ sigint: true });
  const confirm = prompt("This will wipe ALL data in the current database. enter Y to proceed");
  if (confirm != 'Y') return ;

  const code = prompt("Please provide the 42 auth code used for authentication");

    //run this before all testcases
    beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      imports: [AppModule],
    }).compile()
    app = moduleRef.createNestApplication();
    app.useGlobalPipes(new ValidationPipe({
      whitelist: true,
    }));
    await app.init();
    await app.listen(3333);
    prisma = app.get(PrismaService);
    await prisma.cleanDb();
    await prisma.seedSampleUsers();
    pactum.request.setBaseUrl('http://localhost:3001/api/v1')
  })

  it('/ (GET)', () => {
    return pactum
    .spec()
    .get('/')
    .expectStatus(200)
  });

  describe('Auth', () => {
    it(`/auth/authenticate?code=12 (GET)`, () => {
      return pactum
      .spec()
      .get(`/auth/authenticate?code=12`)
      .expectStatus(401)
      .expectBodyContains("error")
      .expectBodyContains("Unauthorized")
    });

    it(`/auth/authenticate?code=${code} (GET)`, () => {
      return pactum
      .spec()
      .get(`/auth/authenticate?code=${code}`)
      .expectStatus(200)
      .expectBodyContains("token")
      .expectBodyContains("expiresAt")
      .stores('token', 'data.token')
    });

    it(`/auth/authenticate?code=${code} (GET)`, () => {
      return pactum
      .spec()
      .get(`/auth/authenticate?code=${code}`)
      .expectStatus(401)
      .expectBodyContains("error")
      .expectBodyContains("Unauthorized")
    });

    it(`/auth/logout (GET) (bad token)`, () => {
      return pactum
      .spec()
      .get(`/auth/logout`)
      .withHeaders({
        Authorization : 'Bearer ererer'
      })
      .expectStatus(403)
    });

  })

  describe('User', () => {
    it(`/users/me (GET)`, () => {
      return pactum
      .spec()
      .get(`/users/me`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectBodyContains("intraID")
    });

    it(`/users/me (PATCH)`, () => {
      const dto = {
        email : "newemail@mail.com"
      }
      return pactum
      .spec()
      .patch(`/users/me`)
      .withBody(dto)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectBodyContains(dto.email)
    });

    it(`/users/me (PATCH)`, () => {
      const dto = {
        email : "newemail2@mail.com",
        level : 0.1
      }
      return pactum
      .spec()
      .patch(`/users/me`)
      .withBody(dto)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectBodyContains(dto.email)
      .expectBodyContains(dto.level)
    });

    it(`/users/me (PATCH)`, () => {
      const dto = {
        intraID : "new"
      }
      return pactum
      .spec()
      .patch(`/users/me`)
      .withBody(dto)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectBodyContains("jng")
    });

    it(`/users/me (PATCH)`, () => {
      const dto = {
        username : "new"
      }
      return pactum
      .spec()
      .patch(`/users/me`)
      .withBody(dto)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectBodyContains(dto.username)
    });

    it(`/users/me (PATCH)`, () => {
      const dto = {
        username : "tes"
      }
      return pactum
      .spec()
      .patch(`/users/me`)
      .withBody(dto)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(400)
    });

    it(`/users (GET) (Pagination)`, () => {
      return pactum
      .spec()
      .get(`/users?page=1&pageSize=1`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectJsonLength("data", 1)
    });

    it(`/users (GET) (Pagination)`, () => {
      return pactum
      .spec()
      .get(`/users?page=2&pageSize=1`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectJsonLength("data", 1)
    });
    
    it(`/users (GET) (Pagination)`, () => {
      return pactum
      .spec()
      .get(`/users?page=3&pageSize=1`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectJsonLength("data", 1)
    });

    it(`/users (GET) (Pagination)`, () => {
      return pactum
      .spec()
      .get(`/users?page=4&pageSize=1`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectJsonLength("data", 0)
    });

    it(`/users (GET) (Pagination)`, () => {
      return pactum
      .spec()
      .get(`/users?page=a&pageSize=1`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(400)
    });

    it(`/users (GET) (Pagination)`, () => {
      return pactum
      .spec()
      .get(`/users?page=1&pageSize=a`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(400)
    });

    it(`/users (GET) (Filtering)`, () => {
      return pactum
      .spec()
      .get(`/users?page=1&pageSize=3&filterOn=status&filterBy=OFFLINE`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectJsonLength("data", 2)
    });

    it(`/users (GET) (Filtering)`, () => {
      return pactum
      .spec()
      .get(`/users?page=1&pageSize=3&filterOn=status,level&filterBy=OFFLINE,1.12`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectJsonLength("data", 2)
    });

    it(`/users (GET) (sorting)`, () => {
      return pactum
      .spec()
      .get(`/users?page=1&pageSize=3&sortBy=Ascending&sortOn=level`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectJsonMatch("data[0]", {level : 0.1})
    });

    it(`/users (GET) (sorting)`, () => {
      return pactum
      .spec()
      .get(`/users?page=1&pageSize=3&sortBy=Descending&sortOn=level`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectJsonMatch("data[0]", {level : 1.12})
    });

    it(`/users (GET) (sorting)`, () => {
      return pactum
      .spec()
      .get(`/users?page=1&pageSize=3&sortBy=Ascending&sortOn=username`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectJsonMatch("data[0]", {username : "new"})
    });

    it(`/users (GET) (sorting)`, () => {
      return pactum
      .spec()
      .get(`/users?page=1&pageSize=3&sortBy=Descendng&sortOn=level`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(400)
    });

    it(`/users (GET) (sorting)`, () => {
      return pactum
      .spec()
      .get(`/users?page=1&pageSize=3&sortBy=Ascending&sortOn=lvel`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(400)
    });

  })

  describe('Auth logout ', () => {

    it(`/auth/logout (GET)`, () => {
      return pactum
      .spec()
      .get(`/auth/logout`)
      .withHeaders({
        Authorization : 'Bearer $S{token}',
      })
      .expectStatus(200)
      .expectBodyContains("token")
    });

  })
  afterAll(()=>{
    app.close();
    prisma.cleanDb();
  })
});
