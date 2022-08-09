import React, { FunctionComponent } from "react";
import { MemberAck } from "./Footer";

interface AckCardProps {
  index: number;
  total: number;
  member: MemberAck;
}

const AckCard: FunctionComponent<AckCardProps> = ({ index, total, member }) => {
  return (
    //   Even
    <div
      className={`grid grid-cols-1 md:grid-cols-6 gap-4 my-5 py-5 ${
        index !== total - 1 ? "border-b-2 border-b-white" : null
      }`}
    >
      <div>
        <img src={member.image} className="rounded-full h-20 w-20" />
      </div>
      <div className="md:col-span-5 flex flex-col justify-center">
        <div className="text-2xl">{member.name}</div>
        <div className="grid grid-rows-2 grid-flow-col">
          <div>
            <a href={member.intraUrl}> @ {member.intraName}</a>
          </div>
          {member.github ? (
            <div>
              <a href={member.github}>
                {" "}
                <img
                  src="/assets/github-sign.png"
                  className="bg-white inline w-4 h-4"
                />{" "}
                {member.github}
              </a>
            </div>
          ) : null}
          {member.linkedin ? (
            <div>
              <a href={member.linkedin}>
                <img
                  src="/assets/linkedin-logo.png"
                  className="bg-white inline w-4 h-4"
                />{" "}
                {member.linkedin}
              </a>
            </div>
          ) : null}
        </div>
      </div>
    </div>
  );
};

export default AckCard;
